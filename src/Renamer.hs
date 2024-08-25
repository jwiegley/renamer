{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Renamer where

import Control.Applicative
import Control.Concurrent.ParallelIO qualified as PIO
import Control.Exception (assert)
import Control.Lens hiding ((<.>))
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Aeson hiding (Error, Options, decodeFileStrict, encodeFile, (.=))
import Data.Aeson qualified as JSON hiding (Error)
import Data.Char (toLower)
import Data.Foldable (foldrM, forM_)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List (group, groupBy, nub, partition, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Time
import Data.Traversable (forM)
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import System.Directory qualified as Dir
import System.Exit
import System.FilePath
import System.Process (Pid, readProcessWithExitCode)
import System.Process qualified as Proc
import Text.Printf
import Text.Regex.TDFA
import Text.Regex.TDFA.String ()
import Text.Show.Pretty (ppShow)
import Prelude hiding (putStrLn)
import Prelude qualified as Pre (putStrLn)

class Has e a where
  within :: Lens' e a

instance Has (a, b) b where
  within = _2

instance Has (a, b) a where
  within = _1

instance Has a a where
  within = id

strToLower :: String -> String
strToLower = Prelude.map toLower

concatMapM :: (Traversable t, Monad m) => (a -> m [b]) -> t a -> m [b]
concatMapM = (fmap concat .) . mapM

duplicatedElements :: (Ord a) => [a] -> [a]
duplicatedElements =
  nub . concat . filter ((> 1) . length) . group . sort

sortAndGroupOn :: (Ord b) => (a -> b) -> [a] -> [NonEmpty a]
sortAndGroupOn f = NE.groupBy ((==) `on` f) . sortOn f

{-------------------------------------------------------------------------
 - Step 1: Schema
 -}

isImage :: String -> Bool
isImage ext = case strToLower ext of
  ".crw" -> True
  ".cr2" -> True
  ".cr3" -> True
  ".dng" -> True
  ".heic" -> True
  ".heif" -> True
  ".tif" -> True
  ".tiff" -> True
  ".psd" -> True
  ".jpg" -> True
  ".jpeg" -> True
  ".png" -> True
  ".webp" -> True
  _ -> False

type Checksum = String

type Extension = String

data FileDetails = FileDetails
  { _captureTime :: Maybe UTCTime,
    _filepath :: FilePath, -- "/foo/bar.CR3"
    _filedir :: FilePath, -- "/foo"
    _filename :: FilePath, -- "bar.CR3"
    _filebase :: FilePath, -- "bar"
    _fileext :: FilePath, -- ".CR3"
    _filesize :: Integer
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses ''FileDetails

instance ToJSON FileDetails where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON FileDetails

type DetailsMap = HashMap FilePath FileDetails

-- These are listed in order of priority, when multiple conflicting renamings
-- are found.
data Renaming
  = SimpleRename UTCTime
  | FollowBase FilePath
  | FollowTime FilePath
  deriving (Eq, Ord, Show, Generic)

makePrisms ''Renaming

instance ToJSON Renaming where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Renaming

data Renamed a = Renamed
  { _sourceDetails :: FileDetails,
    _renamedTo :: a,
    _renaming :: Renaming
  }
  deriving (Eq, Show, Generic)

makeLenses ''Renamed

instance (ToJSON a) => ToJSON (Renamed a) where
  toEncoding = genericToEncoding JSON.defaultOptions

instance (FromJSON a) => FromJSON (Renamed a)

newtype Prefix = Prefix {getPrefix :: FilePath}
  deriving (Eq, Ord, Show, Generic)

sourcePath :: Lens' (Renamed a) FilePath
sourcePath = sourceDetails . filepath

targetPath :: Maybe FilePath -> Renamed FilePath -> FilePath
targetPath destDir r =
  case destDir of
    Just d -> d </> r ^. renamedTo
    Nothing -> r ^. sourceDetails . filedir </> r ^. renamedTo

-- | State of the image repository (new or old).
data RenamerState = RenamerState
  { _dailyCounter :: HashMap String Integer,
    -- | Mapping from file basename to list of entries sharing that basename,
    --   and whatever renaming has been determined for that base
    _entriesAtBase :: HashMap FilePath [FileDetails],
    _renamedEntries :: HashSet FilePath,
    -- | A unique counter used to name temporary files
    _uniqueCounter :: Integer,
    _errorCount :: Integer
  }
  deriving (Show, Generic)

makeLenses ''RenamerState

instance ToJSON RenamerState where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON RenamerState

newRenamerState :: RenamerState
newRenamerState =
  RenamerState
    { _dailyCounter = mempty,
      _entriesAtBase = mempty,
      _renamedEntries = mempty,
      _uniqueCounter = 0,
      _errorCount = 0
    }

{-------------------------------------------------------------------------
 - Step 2: Environment
 -}

data Command
  = ImportPhotos
      { importRepositories :: [FilePath],
        importDestination :: FilePath,
        importInputs :: [FilePath]
      }
  | RenamePhotos
      { renameRepositories :: [FilePath]
      }
  deriving (Show, Eq)

makePrisms ''Command

data Options = Options
  { _quiet :: !Bool,
    _verbose :: !Bool,
    _debug :: !Bool,
    _jobs :: !Int,
    _recursive :: !Bool,
    _execute :: !Bool,
    _caseInsensitive :: !Bool,
    _keepState :: !Bool,
    _spanDirectories :: !Bool,
    _scenarioTo :: !(Maybe FilePath),
    _scenarioFrom :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

makeLenses ''Options

defaultOptions :: Options
defaultOptions =
  Options
    { _quiet = False,
      _verbose = False,
      _debug = False,
      _jobs = 1,
      _recursive = False,
      _execute = False,
      _caseInsensitive = False,
      _keepState = False,
      _spanDirectories = False,
      _scenarioTo = Nothing,
      _scenarioFrom = Nothing
    }

class (Monad m) => MonadLog m where
  printLog :: String -> m ()

instance MonadLog IO where
  printLog = Pre.putStrLn

instance (MonadLog m) => MonadLog (ReaderT e m) where
  printLog = lift . printLog

instance (MonadLog m) => MonadLog (StateT s m) where
  printLog = lift . printLog

data Verbosity
  = Error
  | Normal
  | Verbose
  | Debug

whenDebug :: (MonadReader Options m) => m () -> m ()
whenDebug action = do
  d <- view debug
  when d action

putStrLn_ :: (MonadReader Options m, MonadLog m) => Verbosity -> String -> m ()
putStrLn_ verb s = do
  q <- view quiet
  v <- view verbose
  d <- view debug
  when
    ( case verb of
        Debug -> not q && d
        Verbose -> not q && (d || v)
        Normal -> not q
        Error -> True
    )
    $ printLog s

logErr ::
  ( MonadReader Options m,
    Has e RenamerState,
    MonadState e m,
    MonadLog m
  ) =>
  String ->
  m ()
logErr msg = do
  putStrLn_ Error $ "ERROR: " ++ msg
  within . errorCount += 1

logWarn ::
  ( MonadReader Options m,
    MonadLog m
  ) =>
  String ->
  m ()
logWarn msg = putStrLn_ Normal $ "WARNING: " ++ msg

logErr' ::
  ( MonadReader Options m,
    MonadLog m
  ) =>
  String ->
  m ()
logErr' msg = putStrLn_ Error $ "ERROR: " ++ msg

{-------------------------------------------------------------------------
 - Step 3: Database manipulation
 -}

addToList :: (IxValue a ~ [b], Eq b, At a) => Index a -> b -> a -> a
addToList k v m = case m ^? ix k of
  Just vs
    | v `elem` vs -> m
    | otherwise -> m & ix k %~ (v :)
  Nothing -> m & at k ?~ [v]

setIfMissing :: (IxValue a ~ b, Eq b, At a) => Index a -> b -> a -> a
setIfMissing k v m = case m ^? ix k of
  Just _ -> m
  Nothing -> m & at k ?~ v

nameRe :: String
nameRe = "^([0-9][0-9][0-9][0-9][0-9][0-9])_([0-9][0-9][0-9][0-9])$"

registerCounter ::
  (Has e RenamerState, MonadState e m) =>
  (FilePath -> FilePath) ->
  FilePath ->
  m ()
registerCounter f path = case path =~ nameRe of
  [(_ : ymd : counter : [])] ->
    forM_
      ( parseTimeM
          False
          defaultTimeLocale
          "%0y%0m%0d"
          ymd ::
          Maybe UTCTime
      )
      $ \_ ->
        let ymd' = f ymd
         in preuse (within . dailyCounter . ix ymd') >>= \case
              Just count ->
                within . dailyCounter . at ymd' ?= max count (read counter + 1)
              Nothing ->
                within . dailyCounter . at ymd' ?= read counter + 1
  _ -> pure ()

{-------------------------------------------------------------------------
 - Step 4: Analyze
 -}

class (Monad m) => MonadProc m where
  getCurrentPid :: m Pid

instance MonadProc IO where
  getCurrentPid = Proc.getCurrentPid

instance (MonadProc m) => MonadProc (ReaderT e m) where
  getCurrentPid = lift getCurrentPid

instance (MonadProc m) => MonadProc (StateT s m) where
  getCurrentPid = lift getCurrentPid

class (Monad m) => MonadParallel m where
  parallelInterleaved :: [m a] -> m [a]
  stopGlobalPool :: m ()

instance MonadParallel IO where
  parallelInterleaved = PIO.parallelInterleaved
  stopGlobalPool = PIO.stopGlobalPool

instance (MonadParallel m) => MonadParallel (ReaderT e m) where
  parallelInterleaved xs = do
    e <- ask
    lift $ parallelInterleaved (Prelude.map (flip runReaderT e) xs)
  stopGlobalPool = lift stopGlobalPool

instance (MonadParallel m) => MonadParallel (StateT s m) where
  parallelInterleaved xs = do
    s <- get
    lift $ parallelInterleaved (Prelude.map (flip evalStateT s) xs)
  stopGlobalPool = lift stopGlobalPool

class (Monad m) => MonadPhoto m where
  photoCaptureDate :: FilePath -> m (Maybe UTCTime)

instance MonadPhoto IO where
  photoCaptureDate path =
    runMaybeT $
      exiftoolSubSecDateTimeOriginal path
        <|> exiftoolDateTimeOriginal path

instance (MonadPhoto m) => MonadPhoto (ReaderT e m) where
  photoCaptureDate = lift . photoCaptureDate

instance (MonadPhoto m) => MonadPhoto (StateT s m) where
  photoCaptureDate = lift . photoCaptureDate

exiftoolSubSecDateTimeOriginal :: FilePath -> MaybeT IO UTCTime
exiftoolSubSecDateTimeOriginal path = do
  (ec, out, _err) <-
    liftIO $
      readProcessWithExitCode
        "exiftool"
        ["-SubSecDateTimeOriginal", path]
        ""
  MaybeT $ pure $ case ec of
    ExitSuccess ->
      parseTimeM
        False
        defaultTimeLocale
        "Date/Time Original              : %0Y:%0m:%0d %0H:%0M:%0S%Q%Ez\n"
        out
    ExitFailure _code -> Nothing

exiftoolDateTimeOriginal :: FilePath -> MaybeT IO UTCTime
exiftoolDateTimeOriginal path = do
  (ec, out, _err) <-
    liftIO $
      readProcessWithExitCode
        "exiftool"
        ["-DateTimeOriginal", path]
        ""
  MaybeT $
    pure $
      case ec of
        ExitSuccess ->
          parseTimeM
            False
            defaultTimeLocale
            "Date/Time Original              : %0Y:%0m:%0d %0H:%0M:%0S\n"
            out
        ExitFailure _code -> Nothing

class (Monad m) => MonadFSRead m where
  listDirectory :: FilePath -> m [FilePath]
  doesFileExist :: FilePath -> m Bool
  doesDirectoryExist :: FilePath -> m Bool
  getFileSize :: FilePath -> m Integer

class (Monad m) => MonadFSWrite m where
  removeFile :: FilePath -> m ()
  removeDirectory :: FilePath -> m ()
  renameFile :: FilePath -> FilePath -> m ()
  copyFileWithMetadata :: FilePath -> FilePath -> m ()

instance MonadFSRead IO where
  listDirectory = Dir.listDirectory
  doesFileExist = Dir.doesFileExist
  doesDirectoryExist = Dir.doesDirectoryExist
  getFileSize = Dir.getFileSize

instance (MonadFSRead m) => MonadFSRead (ReaderT e m) where
  listDirectory = lift . listDirectory
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  getFileSize = lift . getFileSize

instance (MonadFSRead m) => MonadFSRead (StateT s m) where
  listDirectory = lift . listDirectory
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  getFileSize = lift . getFileSize

instance MonadFSWrite IO where
  removeFile = Dir.removeFile
  removeDirectory = Dir.removeDirectory
  renameFile = Dir.renameFile
  copyFileWithMetadata = Dir.copyFileWithMetadata

instance (MonadFSWrite m) => MonadFSWrite (ReaderT e m) where
  removeFile = lift . removeFile
  removeDirectory = lift . removeDirectory
  renameFile = (lift .) . renameFile
  copyFileWithMetadata = (lift .) . copyFileWithMetadata

instance (MonadFSWrite m) => MonadFSWrite (StateT s m) where
  removeFile = lift . removeFile
  removeDirectory = lift . removeDirectory
  renameFile = (lift .) . renameFile
  copyFileWithMetadata = (lift .) . copyFileWithMetadata

class (Monad m) => MonadJSON m where
  decodeFileStrict :: (FromJSON a) => FilePath -> m (Maybe a)
  encodeFile :: (ToJSON a) => FilePath -> a -> m ()

instance MonadJSON IO where
  decodeFileStrict = JSON.decodeFileStrict
  encodeFile = JSON.encodeFile

instance (MonadJSON m) => MonadJSON (ReaderT e m) where
  decodeFileStrict = lift . decodeFileStrict
  encodeFile = (lift .) . encodeFile

instance (MonadJSON m) => MonadJSON (StateT s m) where
  decodeFileStrict = lift . decodeFileStrict
  encodeFile = (lift .) . encodeFile

renderDetails ::
  (MonadReader Options m, MonadLog m) =>
  [FileDetails] ->
  m ()
renderDetails = mapM_ $ \d ->
  putStrLn_ Debug $
    d ^. filepath
      ++ maybe "" ((" @ " ++) . show) (d ^. captureTime)

getFileDetails ::
  (MonadPhoto m, MonadFSRead m) =>
  FilePath ->
  m FileDetails
getFileDetails _filepath = do
  isFile <- doesFileExist _filepath
  if isFile
    then do
      _captureTime <-
        if isImage _fileext
          then photoCaptureDate _filepath
          else pure Nothing
      _filesize <- getFileSize _filepath
      pure FileDetails {..}
    else error $ "File does not exist: " ++ _filepath
  where
    _filedir = takeDirectory _filepath
    _filename = takeFileName _filepath
    _filebase = takeBaseName _filepath
    _fileext = takeExtension _filepath

registerFileDetails ::
  (Has e RenamerState, MonadState e m) =>
  Maybe FilePath ->
  FileDetails ->
  m FileDetails
registerFileDetails mdest FileDetails {..} = do
  forM_ mdest $ \destDir ->
    registerCounter (destDir </>) _filebase
  pure FileDetails {..}

walkFileEntries ::
  (MonadFSRead m, MonadLog m) =>
  Bool ->
  (FilePath -> m a) ->
  FilePath ->
  m [m a]
walkFileEntries recurse f path = do
  isDir <- doesDirectoryExist path
  if isDir
    then
      if recurse
        then
          concatMapM (walkFileEntries recurse f . (path </>))
            =<< listDirectory path
        else do
          entries <- map (path </>) <$> listDirectory path
          flip concatMapM entries $ \entry -> do
            dir <- doesDirectoryExist entry
            pure $
              if dir
                then []
                else [f entry]
    else pure [f path]

-- | Takes a list of files and/or directories, and gathers file details for
--   all files involved, recursively.
gatherDetails ::
  ( MonadReader Options m,
    MonadLog m,
    MonadFSRead m,
    MonadJSON m,
    MonadParallel m,
    MonadPhoto m
  ) =>
  [FilePath] ->
  m [FileDetails]
gatherDetails = concatMapM $ \entry -> do
  -- Get info on all entries; this is stateful and builds up the following
  -- tables:
  --   filepathToIdx
  --   idxToFilepath
  recurse <- view recursive
  isDir <- doesDirectoryExist entry
  details <-
    if isDir
      then do
        let detailsFile = entry </> ".file-details.json"
        isFile <- doesFileExist detailsFile
        stateful <- view keepState
        mres <-
          if stateful && isFile
            then decodeFileStrict detailsFile
            else pure Nothing
        case mres of
          Just details -> pure details
          Nothing -> do
            putStrLn_ Normal $ "Gathering details from " ++ show entry
            details <-
              parallelInterleaved
                =<< walkFileEntries recurse getFileDetails entry
            when stateful $
              encodeFile detailsFile details
            pure details
      else do
        putStrLn_ Normal $ "Gathering details from " ++ show entry
        parallelInterleaved
          =<< walkFileEntries recurse getFileDetails entry
  stopGlobalPool
  pure details

processDetails ::
  (Has e RenamerState, MonadState e m) =>
  Maybe FilePath ->
  [FileDetails] ->
  m [FileDetails]
processDetails mdest = mapM (registerFileDetails mdest) . sort

{-------------------------------------------------------------------------
 - Step 5: Naming
 -}

yymmdd :: LocalTime -> String
yymmdd = formatTime defaultTimeLocale "%y%m%d"

nextSeqNum :: (Has e RenamerState, MonadState e m) => String -> m Integer
nextSeqNum ymd =
  preuse (within . dailyCounter . ix ymd) >>= \case
    Just idx -> idx <$ (within . dailyCounter . ix ymd += 1)
    Nothing -> 1 <$ (within . dailyCounter . at ymd ?= 2)

nextUniqueNum :: (Has e RenamerState, MonadState e m) => m Integer
nextUniqueNum = do
  uniqueIdx <- use (within . uniqueCounter)
  within . uniqueCounter += 1
  pure uniqueIdx

normalizeExt :: String -> String
normalizeExt ext = case strToLower ext of
  ".jpeg" -> ".jpg"
  ".tiff" -> ".tif"
  ext' -> ext'

renderRenamings ::
  (MonadReader Options m, MonadLog m) =>
  [Renamed a] ->
  m ()
renderRenamings = mapM_ $ \r ->
  putStrLn_ Debug $
    r ^. sourceDetails . filepath
      ++ " >> "
      ++ show (r ^. renaming)

hasUniqueExts :: [FileDetails] -> Bool
hasUniqueExts =
  null . duplicatedElements . map (normalizeExt . (^. fileext))

-- | Also rename files that are "close" to files being renamed. For example,
--   so that FOO.JPG moves to BAR.JPG when we rename FOO.CR3 to BAR.CR3. This
--   association can happen either due to both files having the same
--   timestamp, or the same basename (which is needed for files that have no
--   capture timestamp, like XMP).
siblingRenamings :: [FileDetails] -> [Renamed FilePath] -> [Renamed FilePath]
siblingRenamings xs = concatMap go
  where
    siblings :: HashMap (FilePath, FilePath) [FileDetails]
    siblings =
      Prelude.foldl'
        (\m x -> addToList (x ^. filedir, x ^. filebase) x m)
        mempty
        xs

    go (Renamed details newname _ren) =
      case siblings
        ^? ix (details ^. filedir, details ^. filebase)
          . to (partition (\y -> y ^. fileext == details ^. fileext)) of
        Just (ys, zs)
          | ys == [details]
              && not (null zs)
              && hasUniqueExts zs
              && all
                ( \z -> case z ^. captureTime of
                    Nothing -> True
                    tm -> tm == details ^. captureTime
                )
                zs ->
              Prelude.map
                ( \z ->
                    Renamed
                      z
                      (name z)
                      (FollowBase (details ^. filepath))
                )
                zs
        _ -> []
      where
        base = takeBaseName newname
        name d = base <.> ext d
        ext d = normalizeExt (d ^. fileext)

expectedPrefix :: TimeZone -> FileDetails -> Maybe FilePath
expectedPrefix tz details = do
  tm <- details ^. captureTime
  pure $ yymmdd (utcToLocalTime tz tm)

targetFilepath :: TimeZone -> FileDetails -> Integer -> FilePath
targetFilepath tz details num = case expectedPrefix tz details of
  Just prefix ->
    details ^. filedir
      </> prefix
      ++ "_"
      ++ printf "%04d" num
        <.> normalizeExt (details ^. fileext)
  Nothing -> details ^. filepath

groupByTime :: Bool -> [FileDetails] -> [[FileDetails]]
groupByTime spanDirs = groupBy $ \x y ->
  case (x ^. captureTime, y ^. captureTime) of
    (Just tmx, Just tmy) ->
      tmx == tmy && (spanDirs || x ^. filedir == y ^. filedir)
    _ -> False

groupByBase :: Bool -> [FileDetails] -> [[FileDetails]]
groupByBase spanDirs = groupBy ((==) `on` dropExtension . pathPart)
  where
    pathPart x
      | spanDirs = x ^. filename
      | otherwise = x ^. filepath

-- If a group has the same time, or the same base, and there is more than one
-- of each file extension, then we cannot be certain and must split up that
-- group, reporting this fact to the user.
splitNonUniqueGroups :: [[FileDetails]] -> [[FileDetails]]
splitNonUniqueGroups = foldr go []
  where
    go xs rest
      | hasUniqueExts xs = xs : rest
      | otherwise = Prelude.map (: []) xs ++ rest

basicRenamings ::
  TimeZone ->
  [FileDetails] ->
  [Maybe (Renamed Prefix)]
basicRenamings tz = Prelude.map $ \details -> do
  tm <- details ^. captureTime
  pure $
    Renamed
      details
      (Prefix (yymmdd (utcToLocalTime tz tm)))
      (SimpleRename tm)

-- | Map photos with capture times to an file having the name YYMMDD_NNNN,
--   where NNNN is a sequence number ordered by time of capture.
--
--   If multiple photos have the same timestamp, and all have different file
--   extensions, then these are all given the same sequence number. If there
--   are overlapping extensions, then all photos are given unique sequence
--   numbers.
--
--   Note that this function aims to be very simple, and so does not perform
--   every possible optimization, such as removing needless renamings that
--   would be idempotent.
simpleRenamings' ::
  TimeZone ->
  [FileDetails] ->
  [Renamed Prefix]
simpleRenamings' tz = concatMap go . M.toAscList . contemporaries
  where
    contemporaries :: [FileDetails] -> Map (FilePath, UTCTime) [FileDetails]
    contemporaries =
      Prelude.foldl'
        ( \m x ->
            maybe
              m
              (\tm -> addToList (x ^. filedir, tm) x m)
              (x ^. captureTime)
        )
        mempty

    go ((_dir, tm), reverse -> entries) = entries'
      where
        entries'
          | hasUniqueExts entries = rename (SimpleRename tm) entries
          | otherwise =
              concatMap (rename (SimpleRename tm) . (: [])) entries

        rename _ [] = []
        rename ren (e : es) =
          work ren e
            : Prelude.map (work (FollowTime (e ^. filename))) es
          where
            work r details = Renamed details (Prefix base) r

            base = yymmdd (utcToLocalTime tz tm)

mapWithPrevM :: (Monad m) => (c -> a -> m (c, b)) -> c -> [a] -> m [b]
mapWithPrevM f = go
  where
    go _ [] = pure []
    go z (x : xs) = do
      (c, y) <- f z x
      (y :) <$> go c xs

applySequenceNumbers ::
  (MonadReader Options m, Has e RenamerState, MonadState e m) =>
  Maybe FilePath ->
  [Renamed Prefix] ->
  m [Renamed FilePath]
applySequenceNumbers mdest = mapWithPrevM go 0
  where
    go lastSeqNum (Renamed d (Prefix n) r) = case r of
      SimpleRename _ -> do
        spanDirs <- view spanDirectories
        num <- nextSeqNum (seqIndex spanDirs n)
        pure (num, Renamed d (name n num) r)
      FollowTime _ ->
        -- jww (2024-08-24): lookup name in a mapping from names to seqnums,
        -- rather than depending on the order.
        pure (lastSeqNum, Renamed d (name n lastSeqNum) r)
      FollowBase _ -> error "Unexpected"
      where
        name base num = base ++ "_" ++ printf "%04d" num <.> ext
        ext = normalizeExt (d ^. fileext)

        seqIndex spanDirs = case mdest of
          Nothing
            | spanDirs -> id
            | otherwise -> (d ^. filedir </>)
          Just destDir -> (destDir </>)

simpleRenamings ::
  (MonadReader Options m, Has e RenamerState, MonadState e m) =>
  TimeZone ->
  Maybe FilePath ->
  [FileDetails] ->
  m [Renamed FilePath]
simpleRenamings tz mdest = applySequenceNumbers mdest . simpleRenamings' tz

-- | Entries that would rename a file to itself.
idempotentRenaming :: Maybe FilePath -> Renamed FilePath -> Bool
idempotentRenaming destDir ren =
  ren ^. sourcePath == targetPath destDir ren

reportIdempotentRenamings ::
  (MonadReader Options m, Has e RenamerState, MonadState e m, MonadLog m) =>
  Maybe FilePath ->
  [Renamed FilePath] ->
  m ()
reportIdempotentRenamings destDir rs =
  forM_ (filter (idempotentRenaming destDir) rs) $ \ren ->
    logErr $
      "Renaming file to itself: " ++ show ren

redundantRenaming ::
  Maybe FilePath ->
  Renamed FilePath ->
  Renamed FilePath ->
  Bool
redundantRenaming destDir rx ry =
  rx ^. sourcePath == ry ^. sourcePath
    && targetPath destDir rx == targetPath destDir ry

removeRedundantRenamings ::
  (Renamed FilePath -> FilePath) ->
  Maybe FilePath ->
  [Renamed FilePath] ->
  [Renamed FilePath]
removeRedundantRenamings f destDir =
  -- Prelude.map NE.head . sortAndGroupOn f
  Prelude.map NE.head . NE.groupBy (redundantRenaming destDir) . sortOn f

groupRenamingsBy ::
  (Renamed FilePath -> FilePath) ->
  [Renamed FilePath] ->
  [NonEmpty (Renamed FilePath)]
groupRenamingsBy f = filter (\xs -> NE.length xs > 1) . sortAndGroupOn f

removeOverlappedRenamings ::
  Maybe FilePath ->
  [Renamed FilePath] ->
  ([Renamed FilePath], [(Renamed FilePath, NonEmpty (Renamed FilePath))])
removeOverlappedRenamings destDir rs =
  ( Prelude.map fst rs'',
    onlyOverlaps rs' ++ onlyOverlaps rs''
  )
  where
    rs' = nonOverlapped (^. sourcePath) rs
    rs'' = nonOverlapped (targetPath destDir) (Prelude.map fst rs')

    nonOverlapped f =
      foldr
        ( \rens rest ->
            let k r = case r ^. renaming of
                  FollowBase _ ->
                    r ^. sourceDetails . filepath
                      `notElem` rest ^.. traverse . _1 . renaming . _FollowBase
                  _ -> True
             in -- We can't do a trivial sort here, which might select
                -- FollowsBase for two entries that each follow each other.
                -- Instead, it must take into account earlier renamings that
                -- have been chosen. If
                --   A [B]-> C   A (T)-> D
                --   B [A]-> E   B (U)-> F
                -- then we should end up with
                --   A [B]-> C   B (U)-> F
                -- where a simple sort would have chosen
                --   A [B]-> C   B [A]-> E
                case sortOn (^. renaming) (NE.filter k rens) of
                  [] -> error "Unexpected: removeOverlappedRenamings"
                  y : ys -> (y, ys) : rest
        )
        []
        . sortAndGroupOn f

    onlyOverlaps = concatMap $ \(x, xs) -> case xs of
      [] -> []
      y : ys -> [(x, y :| ys)]

reportOverlappedSources ::
  (MonadReader Options m, Has e RenamerState, MonadState e m, MonadLog m) =>
  Maybe FilePath ->
  [Renamed FilePath] ->
  m ()
reportOverlappedSources destDir rs =
  forM_ (groupRenamingsBy (^. sourcePath) rs) $ \dsts ->
    forM_ dsts $ \dst ->
      logErr $
        "Overlapped source: "
          ++ dst ^. sourcePath
          ++ " -> "
          ++ targetPath destDir dst

reportOverlappedTargets ::
  (MonadReader Options m, Has e RenamerState, MonadState e m, MonadLog m) =>
  Maybe FilePath ->
  [Renamed FilePath] ->
  m ()
reportOverlappedTargets destDir rs =
  forM_ (groupRenamingsBy (targetPath destDir) rs) $ \srcs ->
    forM_ srcs $ \src ->
      logErr $
        "Overlapped target: "
          ++ src ^. sourcePath
          ++ " -> "
          ++ targetPath destDir src

renamingLabel :: TimeZone -> Renamed FilePath -> FilePath -> FilePath -> String
renamingLabel tz ren srcPath dstPath =
  srcPath
    ++ case ren ^. renaming of
      SimpleRename tm -> " (" ++ formattedTime tm ++ ")-> "
      FollowBase name -> " [" ++ name ++ "]-> "
      FollowTime name -> " {" ++ name ++ "}-> "
    ++ dstPath
  where
    formattedTime tm =
      formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" tm'
      where
        tm' = utcToLocalTime tz tm

data RenamingSet = RenamingSet
  { _allSimpleRenamings :: [Renamed FilePath],
    _allSiblingRenamings :: [Renamed FilePath],
    _allRenamingsWithoutRedundancies :: [Renamed FilePath],
    -- | All remainings includes both simple and sibling renamings, and is
    --   clear of idempotent, redundant and overlapped renamings.
    _allRenamings :: [Renamed FilePath]
  }
  deriving (Eq, Show, Generic)

makeLenses ''RenamingSet

instance ToJSON RenamingSet where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON RenamingSet

newRenamingSet :: RenamingSet
newRenamingSet =
  RenamingSet
    { _allSimpleRenamings = [],
      _allSiblingRenamings = [],
      _allRenamingsWithoutRedundancies = [],
      _allRenamings = []
    }

-- | Determine the ideal name for a given photo, in the context of the
--   repository where it is meant to abide. Note that this function is called
--   only after all file details have been gathered throughout the various
--   photo repositories.
--
--   1. It should contain the date when the photo was taken.
--
--   2. It should contain the sequence of which photo it was that day.
--
--   3. If there are two files with the same basename but different
--      extensions, they should be renamed together.
--
--   4. If it is the alternate version (different extension) of an existing
--      photo, it should share the sequence number.
renameFiles ::
  (MonadReader Options m, Has e RenamerState, MonadState e m, MonadLog m) =>
  TimeZone ->
  Maybe FilePath ->
  [FileDetails] ->
  m RenamingSet
renameFiles tz destDir ds = do
  rs1 <- simpleRenamings tz destDir ds
  let rs1' = filter (not . idempotentRenaming destDir) rs1
      rs2 = siblingRenamings ds rs1'
      rs3 = rs1' ++ rs2
  assert
    ( Prelude.all
        (\g -> length (NE.filter (has (renaming . _SimpleRename)) g) < 2)
        (groupRenamingsBy (^. sourcePath) rs3)
    )
    $ do
      let rs4 =
            removeRedundantRenamings (targetPath destDir) destDir $
              removeRedundantRenamings (^. sourcePath) destDir $
                filter (not . idempotentRenaming destDir) $
                  rs3
          (rs5, overlaps) = removeOverlappedRenamings destDir rs4
      forM_ overlaps $ \(x, ys) -> do
        putStrLn_ Normal $ "Preferring this renaming:"
        putStrLn_ Normal $
          "    "
            ++ renamingLabel tz x (x ^. sourcePath) (targetPath destDir x)
        putStrLn_ Normal $ "  over these:"
        forM_ ys $ \y ->
          putStrLn_ Normal $
            "    "
              ++ renamingLabel tz y (y ^. sourcePath) (targetPath destDir y)
      pure $ RenamingSet rs1 rs2 rs4 rs5

{-------------------------------------------------------------------------
 - Step 6: Plan
 -}

data Mapping = Mapping
  { _sourceFile :: FilePath,
    _targetFile :: FilePath,
    _renamingRef :: Renamed FilePath
  }
  deriving (Eq, Show, Generic)

makeLenses ''Mapping

instance ToJSON Mapping where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Mapping

renderMappings ::
  ( MonadReader Options m,
    Has e RenamerState,
    MonadState e m,
    MonadLog m
  ) =>
  [Mapping] ->
  m ()
renderMappings = mapM_ $ \(Mapping src dst _) ->
  putStrLn_ Debug $ src ++ " >>> " ++ dst

buildBasicPlan ::
  Maybe FilePath ->
  [Renamed FilePath] ->
  [Mapping]
buildBasicPlan destDir = Prelude.map $ \ren ->
  Mapping (ren ^. sourcePath) (targetPath destDir ren) ren

safeguardPlan ::
  (Has e RenamerState, MonadState e m, MonadProc m, MonadLog m) =>
  [Mapping] ->
  m [Mapping]
safeguardPlan plan = do
  pid <- getCurrentPid
  (_, xs, ys) <- foldrM (work pid) (mempty, [], []) plan
  pure $ xs ++ ys
  where
    work pid (Mapping src dst ren) (srcs, rest, post)
      | dst `S.member` srcs = do
          uniqueIdx <- nextUniqueNum
          let tmp =
                takeDirectory dst
                  </> "tmp_"
                  ++ show pid
                  ++ "_"
                  ++ show uniqueIdx
          pure
            ( srcs & at src ?~ (),
              Mapping src tmp ren : rest,
              Mapping tmp dst ren : post
            )
      | otherwise =
          pure
            ( srcs & at src ?~ (),
              Mapping src dst ren : rest,
              post
            )

buildPlan ::
  ( MonadReader Options m,
    Has e RenamerState,
    MonadState e m,
    MonadProc m,
    MonadLog m
  ) =>
  Maybe FilePath ->
  [Renamed FilePath] ->
  m [Mapping]
buildPlan destDir rs = do
  reportIdempotentRenamings destDir rs
  reportOverlappedSources destDir rs
  reportOverlappedTargets destDir rs
  safeguardPlan (buildBasicPlan destDir rs)

{-------------------------------------------------------------------------
 - Step 7: Execute
 -}

data Scenario = Scenario
  { _scenarioPid :: Int,
    _scenarioTimeZoneMinutes :: Int,
    _scenarioRepository :: [FileDetails],
    _scenarioDestination :: Maybe FilePath,
    _scenarioInputs :: [FileDetails],
    _scenarioRenamings :: RenamingSet,
    _scenarioMappings :: [Mapping]
  }
  deriving (Show, Generic)

makeLenses ''Scenario

instance ToJSON Scenario where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Scenario

newScenario :: Scenario
newScenario =
  Scenario
    { _scenarioPid = 0,
      _scenarioTimeZoneMinutes = 0,
      _scenarioRepository = [],
      _scenarioDestination = Nothing,
      _scenarioInputs = [],
      _scenarioRenamings = newRenamingSet,
      _scenarioMappings = []
    }

determineScenario ::
  ( MonadReader Options m,
    Has e RenamerState,
    MonadState e m,
    MonadProc m,
    MonadFSRead m,
    MonadJSON m,
    MonadParallel m,
    MonadPhoto m,
    MonadLog m
  ) =>
  TimeZone ->
  [FilePath] ->
  [FilePath] ->
  Maybe FilePath ->
  m Scenario
determineScenario
  tz
  _scenarioRepositories
  _scenarioInputs
  _scenarioDestination = do
    _scenarioPid <- fromIntegral <$> getCurrentPid
    let _scenarioTimeZoneMinutes = timeZoneMinutes tz
    (_scenarioRepository, _scenarioInputs) <- doGatherDetails
    _scenarioRenamings <-
      doRenameFiles
        ( if null _scenarioInputs
            then _scenarioRepository
            else _scenarioInputs
        )
    _scenarioMappings <- doBuildPlan (_scenarioRenamings ^. allRenamings)
    pure Scenario {..}
    where
      doGatherDetails = do
        putStrLn_ Normal "Gathering details..."
        rds <-
          if null _scenarioRepositories
            then pure []
            else
              gatherDetails _scenarioRepositories
                >>= processDetails _scenarioDestination
        ds <-
          gatherDetails _scenarioInputs
            >>= processDetails _scenarioDestination
        whenDebug $ renderDetails rds
        whenDebug $ renderDetails ds
        pure (rds, ds)

      doRenameFiles details = do
        putStrLn_ Normal $
          "Determining expected file names (from "
            ++ show (length details)
            ++ " entries)..."
        rs <- renameFiles tz _scenarioDestination details
        whenDebug $ renderRenamings (rs ^. allRenamings)
        pure rs

      doBuildPlan renamings = do
        putStrLn_ Normal $
          "Building renaming plan (from "
            ++ show (length renamings)
            ++ " renamings)..."
        p <- buildPlan _scenarioDestination renamings
        whenDebug $ renderMappings p
        pure p

safeRemoveDirectory ::
  (MonadReader Options m, MonadLog m, MonadFSWrite m) =>
  FilePath ->
  m ()
safeRemoveDirectory path = do
  putStrLn_ Normal $ "- " ++ path
  removeDirectory path

safePruneDirectory ::
  (MonadReader Options m, MonadLog m, MonadFSRead m, MonadFSWrite m) =>
  FilePath ->
  m ()
safePruneDirectory path = do
  entries <- listDirectory path
  safeToRemove <- flip execStateT True $
    forM_ entries $ \entry -> do
      let entryPath = path </> entry
      isDir <- lift $ doesDirectoryExist entryPath
      if isDir
        then lift $ safePruneDirectory entryPath
        else put False
  when safeToRemove $
    safeRemoveDirectory path

safeRemoveFile ::
  (MonadReader Options m, MonadLog m, MonadFSWrite m) =>
  FilePath ->
  m ()
safeRemoveFile path = do
  -- putStrLn_ Debug $ "- " ++ path
  removeFile path

safeMoveFile ::
  ( MonadReader Options m,
    MonadLog m,
    MonadFSRead m,
    MonadFSWrite m
  ) =>
  (FilePath -> FilePath -> String) ->
  FilePath ->
  FilePath ->
  m Integer
safeMoveFile label src dst
  | strToLower src == strToLower dst = do
      putStrLn_ Verbose $ src ++ " => " ++ dst
      renameFile src dst
      pure 0
  | otherwise = do
      putStrLn_ Verbose $ label src dst
      isFile <- doesFileExist dst
      if isFile
        then do
          logWarn $
            "Destination already exists, appending + suffix: "
              ++ label src dst
          safeMoveFile label src (dropExtension dst ++ "+" ++ takeExtension dst)
        else do
          copyFileWithMetadata src dst
          safeRemoveFile src
          pure 0

executePlan ::
  ( MonadReader Options m,
    MonadLog m,
    MonadFSRead m,
    MonadFSWrite m
  ) =>
  TimeZone ->
  [Mapping] ->
  m Integer
executePlan tz plan = do
  putStrLn_ Normal $
    "Executing renaming plan ("
      ++ show (length plan)
      ++ " operations)..."
  results <- forM plan $ \(Mapping src dst ren) ->
    safeMoveFile (renamingLabel tz ren) src dst
  let errors = sum results
  putStrLn_ Normal $ "Renaming completed with " ++ show errors ++ " errors"
  pure errors

renamerExecute ::
  ( MonadReader Options m,
    Has e RenamerState,
    MonadState e m,
    MonadLog m,
    MonadFSRead m,
    MonadFSWrite m
  ) =>
  TimeZone ->
  Scenario ->
  m Integer
renamerExecute tz scenario = do
  errors <- use (within . errorCount)
  if errors > 0
    then errors <$ logErr "Cannot execute renaming plan with errors"
    else executePlan tz (scenario ^. scenarioMappings)
