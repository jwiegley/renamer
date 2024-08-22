{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Renamer where

import Control.Applicative
import Control.Concurrent.ParallelIO qualified as PIO
import Control.Exception (assert)
import Control.Lens hiding ((<.>))
import Control.Monad (unless, when)
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
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as S
import Data.List (group, nub, partition, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Time
import GHC.Generics (Generic)
import System.Directory qualified as Dir
import System.Exit
import System.FilePath
import System.Process (Pid, readProcessWithExitCode)
import System.Process qualified as Proc
import Text.Printf
import Text.Regex.TDFA
import Text.Regex.TDFA.String ()
import Prelude hiding (putStrLn)
import Prelude qualified as Pre (putStrLn)

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
    _fileIdx :: Int,
    _fileext :: FilePath, -- ".CR3"
    _checksum :: Maybe Checksum, -- "<hex string>"
    _fileSize :: Integer
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
  = SimpleRenameAvoidOverlap UTCTime
  | SimpleRename UTCTime
  | FollowBase FilePath
  | FollowTime FilePath
  deriving (Eq, Ord, Show, Generic)

makePrisms ''Renaming

instance ToJSON Renaming where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Renaming

data RenamedFile = RenamedFile
  { _sourceDetails :: FileDetails,
    _renamedFile :: FilePath,
    _renaming :: Renaming
  }
  deriving (Eq, Show, Generic)

makeLenses ''RenamedFile

instance ToJSON RenamedFile where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON RenamedFile

source :: Lens' RenamedFile FilePath
source = sourceDetails . filepath

target :: Maybe FilePath -> RenamedFile -> FilePath
target destDir r =
  case destDir of
    Just d -> d </> r ^. renamedFile
    Nothing -> r ^. sourceDetails . filedir </> r ^. renamedFile

-- | State of the image repository (new or old).
data RenamerState = RenamerState
  { -- | Mapping from full pathnames to unique integer identifiers and back
    _filepathToIdx :: HashMap FilePath Int,
    _idxToFilepath :: IntMap (FilePath, Maybe Checksum),
    _fileIdxCounter :: Int,
    -- | Mapping from YYmmdd to a sequence counter for that day
    _dailyCounter :: HashMap String Int,
    -- | Mapping from file basename to list of entries sharing that basename,
    --   and whatever renaming has been determined for that base
    _entriesAtBase :: HashMap FilePath [FileDetails],
    -- | Mapping from checksums to a list of entries sharing that checksum
    _fileChecksums :: HashMap Checksum [FilePath],
    _renamedEntries :: HashSet FilePath,
    -- | A unique counter used to name temporary files
    _uniqueCounter :: Int,
    _errorCount :: Int
  }
  deriving (Show, Generic)

makeLenses ''RenamerState

instance ToJSON RenamerState where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON RenamerState

newRenamerState :: RenamerState
newRenamerState =
  RenamerState
    { _filepathToIdx = mempty,
      _idxToFilepath = mempty,
      _fileIdxCounter = 0,
      _dailyCounter = mempty,
      _entriesAtBase = mempty,
      _fileChecksums = mempty,
      _renamedEntries = mempty,
      _uniqueCounter = 0,
      _errorCount = 0
    }

{-------------------------------------------------------------------------
 - Step 2: Environment
 -}

data Command
  = ImportPhotos [FilePath] FilePath [FilePath]
  | RenamePhotos [FilePath]
  deriving (Show, Eq)

makePrisms ''Command

data Options = Options
  { _quiet :: !Bool,
    _verbose :: !Bool,
    _debug :: !Bool,
    _jobs :: !Int,
    _checksums :: !Bool,
    _recursive :: !Bool,
    _execute :: !Bool,
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
      _checksums = False,
      _recursive = False,
      _execute = False,
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
    MonadState RenamerState m,
    MonadLog m
  ) =>
  String ->
  m ()
logErr msg = do
  putStrLn_ Error $ "ERROR: " ++ msg
  errorCount += 1

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

-- | Register a path name, return its unique integer identifier.
registerPath ::
  (MonadState RenamerState m) =>
  FilePath ->
  Maybe Checksum ->
  m Int
registerPath path mcsum = do
  forM_ mcsum $ \csum ->
    fileChecksums %= addToList csum path

  preuse (filepathToIdx . ix path) >>= \case
    Just idx -> pure idx
    Nothing -> do
      idx <- use fileIdxCounter
      fileIdxCounter += 1
      filepathToIdx . at path ?= idx
      idxToFilepath . at idx ?= (path, mcsum)
      pure idx

nameRe :: String
nameRe = "^([0-9][0-9][0-9][0-9][0-9][0-9])_([0-9][0-9][0-9][0-9])$"

registerCounter ::
  (MonadState RenamerState m) =>
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
         in preuse (dailyCounter . ix ymd') >>= \case
              Just count ->
                dailyCounter . at ymd' ?= max count (read counter + 1)
              Nothing ->
                dailyCounter . at ymd' ?= read counter + 1
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

class (Monad m) => MonadChecksum m where
  fileChecksum :: FilePath -> m String

instance MonadChecksum IO where
  fileChecksum = b3sum

instance (MonadChecksum m) => MonadChecksum (ReaderT e m) where
  fileChecksum = lift . fileChecksum

instance (MonadChecksum m) => MonadChecksum (StateT s m) where
  fileChecksum = lift . fileChecksum

class (Monad m) => MonadPhoto m where
  photoCaptureDate :: FilePath -> m (Maybe UTCTime)

instance MonadPhoto IO where
  photoCaptureDate path =
    runMaybeT $
      exiftoolSubSecDateTimeOriginal path
        <|> exiftoolDateTimeOriginal path
        <|> exiv2ImageTimestamp path

instance (MonadPhoto m) => MonadPhoto (ReaderT e m) where
  photoCaptureDate = lift . photoCaptureDate

instance (MonadPhoto m) => MonadPhoto (StateT s m) where
  photoCaptureDate = lift . photoCaptureDate

b3sum :: FilePath -> IO String
b3sum path = do
  (ec, out, err) <-
    readProcessWithExitCode
      "b3sum"
      ["--no-names", "--quiet", path]
      ""
  case ec of
    ExitSuccess -> pure $ init out
    ExitFailure code ->
      error $ "b3sum failed, code " ++ show code ++ ": " ++ err

exiv2ImageTimestamp :: FilePath -> MaybeT IO UTCTime
exiv2ImageTimestamp path = do
  (ec, out, _err) <-
    liftIO $
      readProcessWithExitCode
        "exiv2"
        ["-g", "Exif.Image.DateTime", "-Pv", path]
        ""
  MaybeT $
    pure $
      case ec of
        ExitSuccess ->
          parseTimeM
            False
            defaultTimeLocale
            "%0Y:%0m:%0d %0H:%0M:%0S\n"
            out
        ExitFailure _code -> Nothing

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

class (Monad m) => MonadFS m where
  listDirectory :: FilePath -> m [FilePath]
  doesFileExist :: FilePath -> m Bool
  doesDirectoryExist :: FilePath -> m Bool
  getFileSize :: FilePath -> m Integer
  removeFile :: FilePath -> m ()
  removeDirectory :: FilePath -> m ()
  renameFile :: FilePath -> FilePath -> m ()
  copyFileWithMetadata :: FilePath -> FilePath -> m ()

instance MonadFS IO where
  listDirectory = Dir.listDirectory
  doesFileExist = Dir.doesFileExist
  doesDirectoryExist = Dir.doesDirectoryExist
  getFileSize = Dir.getFileSize
  removeFile = Dir.removeFile
  removeDirectory = Dir.removeDirectory
  renameFile = Dir.renameFile
  copyFileWithMetadata = Dir.copyFileWithMetadata

instance (MonadFS m) => MonadFS (ReaderT e m) where
  listDirectory = lift . listDirectory
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  getFileSize = lift . getFileSize
  removeFile = lift . removeFile
  removeDirectory = lift . removeDirectory
  renameFile = (lift .) . renameFile
  copyFileWithMetadata = (lift .) . copyFileWithMetadata

instance (MonadFS m) => MonadFS (StateT s m) where
  listDirectory = lift . listDirectory
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  getFileSize = lift . getFileSize
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
  (MonadReader Options m, MonadLog m, MonadFail m) =>
  [FileDetails] ->
  m ()
renderDetails = mapM_ $ \d ->
  putStrLn_ Debug $
    d ^. filepath
      ++ maybe "" ((" @ " ++) . show) (d ^. captureTime)

getFileDetails ::
  (Alternative m, MonadChecksum m, MonadPhoto m, MonadFS m) =>
  Bool ->
  FilePath ->
  m FileDetails
getFileDetails computeChecksum path = do
  isFile <- doesFileExist path
  unless isFile $
    error $
      "File does not exist: " ++ path
  _checksum <-
    if computeChecksum
      then Just <$> fileChecksum path
      else pure Nothing
  let _filepath = path
      _filedir = takeDirectory path
      _filename = takeFileName path
      _filebase = takeBaseName path
      _fileext = takeExtension path
  _captureTime <-
    if isImage _fileext
      then photoCaptureDate path
      else pure Nothing
  let _fileIdx = -1
  _fileSize <- getFileSize path
  pure FileDetails {..}

registerFileDetails ::
  (MonadState RenamerState m) =>
  Maybe FilePath ->
  FileDetails ->
  m FileDetails
registerFileDetails mdest FileDetails {..} = do
  forM_ mdest $ \destDir ->
    registerCounter (destDir </>) _filebase
  _fileIdx <- registerPath (_filedir </> _filename) _checksum
  pure FileDetails {..}

walkFileEntries ::
  (MonadFS m, MonadLog m) =>
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
    MonadFS m,
    MonadJSON m,
    MonadParallel m,
    MonadChecksum m,
    MonadPhoto m,
    Alternative m
  ) =>
  [FilePath] ->
  m [FileDetails]
gatherDetails = concatMapM $ \entry -> do
  -- Get info on all entries; this is stateful and builds up the following
  -- tables:
  --   filepathToIdx
  --   idxToFilepath
  --   fileChecksums
  c <- view checksums
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
                =<< walkFileEntries recurse (getFileDetails c) entry
            when stateful $
              encodeFile detailsFile details
            pure details
      else do
        putStrLn_ Normal $ "Gathering details from " ++ show entry
        parallelInterleaved
          =<< walkFileEntries recurse (getFileDetails c) entry
  stopGlobalPool
  pure details

processDetails ::
  (MonadState RenamerState m) =>
  Maybe FilePath ->
  [FileDetails] ->
  m [FileDetails]
processDetails mdest = mapM (registerFileDetails mdest)

{-------------------------------------------------------------------------
 - Step 5: Naming
 -}

yymmdd :: LocalTime -> String
yymmdd = formatTime defaultTimeLocale "%y%m%d"

nextSeqNum :: forall m. (MonadState RenamerState m) => String -> m Int
nextSeqNum ymd =
  preuse (dailyCounter . ix ymd) >>= \case
    Just idx -> idx <$ (dailyCounter . ix ymd += 1)
    Nothing -> 1 <$ (dailyCounter . at ymd ?= 2)

nextUniqueNum :: (MonadState RenamerState m) => m Int
nextUniqueNum = do
  uniqueIdx <- use uniqueCounter
  uniqueCounter += 1
  pure uniqueIdx

normalizeExt :: String -> String
normalizeExt ext = case strToLower ext of
  ".jpeg" -> ".jpg"
  ".tiff" -> ".tif"
  ext' -> ext'

renderRenamings ::
  (MonadReader Options m, MonadLog m, MonadFail m) =>
  [RenamedFile] ->
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
siblingRenamings :: [FileDetails] -> [RenamedFile] -> [RenamedFile]
siblingRenamings xs = concatMap go
  where
    siblings :: HashMap (FilePath, FilePath) [FileDetails]
    siblings =
      Prelude.foldl'
        (\m x -> addToList (x ^. filedir, x ^. filebase) x m)
        mempty
        xs

    go (RenamedFile details newname _ren) =
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
                    RenamedFile
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
simpleRenamings ::
  (MonadReader Options m, MonadState RenamerState m) =>
  TimeZone ->
  Maybe FilePath ->
  [FileDetails] ->
  m [RenamedFile]
simpleRenamings tz mdest =
  fmap reverse . foldrM go [] . M.toDescList . contemporaries
  where
    contemporaries ::
      [FileDetails] -> Map (FilePath, UTCTime) [FileDetails]
    contemporaries =
      Prelude.foldl'
        ( \m x ->
            maybe
              m
              (\tm -> addToList (x ^. filedir, tm) x m)
              (x ^. captureTime)
        )
        mempty

    go ((_dir, tm), entries) rest = do
      entries' <-
        if hasUniqueExts entries
          then rename (SimpleRename tm) entries
          else
            concatMapM
              (rename (SimpleRenameAvoidOverlap tm) . (: []))
              entries
      pure $ entries' ++ rest
      where
        rename _ [] = pure []
        rename f (e : es) = do
          base <- expectedBase
          pure $
            work f base e []
              ++ foldr (work (FollowTime (e ^. filename)) base) [] es
          where
            work ren base details =
              (RenamedFile details (name details) ren :)
              where
                name d = base <.> ext d
                ext d = normalizeExt (d ^. fileext)

            expectedBase = do
              spanDirs <- view spanDirectories
              newName (yymmdd tm')
                <$> nextSeqNum (seqIndex spanDirs (yymmdd tm'))
              where
                seqIndex spanDirs = case mdest of
                  Nothing
                    | spanDirs -> id
                    | otherwise -> (e ^. filedir </>)
                  Just destDir -> (destDir </>)

                tm' = utcToLocalTime tz tm
                newName base seqNum = base ++ "_" ++ printf "%04d" seqNum

-- | Entries that would rename a file to itself.
idempotentRenaming :: Maybe FilePath -> RenamedFile -> Bool
idempotentRenaming destDir ren = ren ^. source == target destDir ren

reportIdempotentRenamings ::
  (MonadReader Options m, MonadState RenamerState m, MonadLog m) =>
  Maybe FilePath ->
  [RenamedFile] ->
  m ()
reportIdempotentRenamings destDir rs =
  forM_ (filter (idempotentRenaming destDir) rs) $ \ren ->
    logErr $
      "Renaming file to itself: " ++ show ren

redundantRenaming :: Maybe FilePath -> RenamedFile -> RenamedFile -> Bool
redundantRenaming destDir rx ry =
  rx ^. source == ry ^. source && target destDir rx == target destDir ry

removeRedundantRenamings ::
  (RenamedFile -> FilePath) -> Maybe FilePath -> [RenamedFile] -> [RenamedFile]
removeRedundantRenamings f destDir =
  -- Prelude.map NE.head . sortAndGroupOn f
  Prelude.map NE.head . NE.groupBy (redundantRenaming destDir) . sortOn f

groupRenamingsBy ::
  (RenamedFile -> FilePath) ->
  [RenamedFile] ->
  [NonEmpty RenamedFile]
groupRenamingsBy f = filter (\xs -> NE.length xs > 1) . sortAndGroupOn f

removeOverlappedRenamings ::
  Maybe FilePath ->
  [RenamedFile] ->
  ([RenamedFile], [(RenamedFile, NonEmpty RenamedFile)])
removeOverlappedRenamings destDir rs =
  ( Prelude.map fst rs'',
    onlyOverlaps rs' ++ onlyOverlaps rs''
  )
  where
    rs' = nonOverlapped (^. source) rs
    rs'' = nonOverlapped (target destDir) (Prelude.map fst rs')

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
                  [] -> error "Unexpected in removeOverlappedRenamings"
                  y : ys -> (y, ys) : rest
        )
        []
        . sortAndGroupOn f

    onlyOverlaps = concatMap $ \(x, xs) -> case xs of
      [] -> []
      y : ys -> [(x, y :| ys)]

reportOverlappedSources ::
  (MonadReader Options m, MonadState RenamerState m, MonadLog m) =>
  Maybe FilePath ->
  [RenamedFile] ->
  m ()
reportOverlappedSources destDir rs =
  forM_ (groupRenamingsBy (^. source) rs) $ \dsts ->
    forM_ dsts $ \dst ->
      logErr $
        "Overlapped source: "
          ++ dst ^. source
          ++ " -> "
          ++ target destDir dst

reportOverlappedTargets ::
  (MonadReader Options m, MonadState RenamerState m, MonadLog m) =>
  Maybe FilePath ->
  [RenamedFile] ->
  m ()
reportOverlappedTargets destDir rs =
  forM_ (groupRenamingsBy (target destDir) rs) $ \srcs ->
    forM_ srcs $ \src ->
      logErr $
        "Overlapped target: "
          ++ src ^. source
          ++ " -> "
          ++ target destDir src

renamingLabel :: TimeZone -> RenamedFile -> FilePath -> FilePath -> String
renamingLabel tz ren srcPath dstPath =
  srcPath
    ++ case ren ^. renaming of
      SimpleRename tm -> " (" ++ formattedTime tm ++ ")-> "
      SimpleRenameAvoidOverlap tm -> " (" ++ formattedTime tm ++ ")!> "
      FollowBase name -> " [" ++ name ++ "]-> "
      FollowTime name -> " {" ++ name ++ "}-> "
    ++ dstPath
  where
    formattedTime tm =
      formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" tm'
      where
        tm' = utcToLocalTime tz tm

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
  (MonadReader Options m, MonadState RenamerState m, MonadLog m) =>
  TimeZone ->
  Maybe FilePath ->
  -- | Map over the simple renamings
  ([RenamedFile] -> m [RenamedFile]) ->
  -- | Map over the siblings renamings (following base)
  ([RenamedFile] -> m [RenamedFile]) ->
  -- | Map over both of the above, with idempotents removed from the simple
  --   set; this constitutes all possible renamings before redundant and
  --   overlapping renamings are removed.
  ([RenamedFile] -> m [RenamedFile]) ->
  -- | Map over the above after idempotent and redundant renamings are
  --   removed.
  ([RenamedFile] -> m [RenamedFile]) ->
  -- | Map over the above after overlapped remainings have been removed. This
  --   is the final renaming set. The reason to have all of these interception
  --   functions is so that the test framework can have visibility into the
  --   nature of the set at each stage, without replicating this logic.
  ([RenamedFile] -> m [RenamedFile]) ->
  [FileDetails] ->
  m [RenamedFile]
renameFiles tz destDir k1 k2 k3 k4 k5 ds = do
  rs1 <- k1 =<< simpleRenamings tz destDir ds
  let rs1' = filter (not . idempotentRenaming destDir) rs1
  rs2 <- k2 (siblingRenamings ds rs1')
  rs3 <- k3 (rs1' ++ rs2)
  assert
    ( Prelude.all
        ( \g ->
            length
              ( NE.filter
                  ( \x ->
                      has (renaming . _SimpleRenameAvoidOverlap) x
                        || has (renaming . _SimpleRename) x
                  )
                  g
              )
              < 2
        )
        (groupRenamingsBy (^. source) rs3)
    )
    $ do
      rs4 <-
        k4
          ( removeRedundantRenamings (target destDir) destDir $
              removeRedundantRenamings (^. source) destDir $
                filter (not . idempotentRenaming destDir) $
                  rs3
          )
      let (rs5, overlaps) = removeOverlappedRenamings destDir rs4
      forM_ overlaps $ \(x, ys) -> do
        putStrLn_ Normal $ "Preferring this renaming:"
        putStrLn_ Normal $
          "    "
            ++ renamingLabel tz x (x ^. source) (target destDir x)
        putStrLn_ Normal $ "  over these:"
        forM_ ys $ \y ->
          putStrLn_ Normal $
            "    "
              ++ renamingLabel tz y (y ^. source) (target destDir y)
      k5 rs5

{-------------------------------------------------------------------------
 - Step 6: Plan
 -}

data Mapping = Mapping
  { _sourceFileIdx :: Int,
    _targetFileIdx :: Int,
    _renamingRef :: RenamedFile
  }
  deriving (Show, Generic)

makeLenses ''Mapping

instance ToJSON Mapping where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Mapping

mappingSets :: [Mapping] -> (IntSet, IntSet)
mappingSets = foldl' go (mempty, mempty)
  where
    go (srcSet, dstSet) (Mapping src dst _) =
      (srcSet & at src ?~ (), dstSet & at dst ?~ ())

renderMappings ::
  ( MonadReader Options m,
    MonadState RenamerState m,
    MonadLog m,
    MonadFail m
  ) =>
  [Mapping] ->
  m ()
renderMappings = mapM_ $ \(Mapping src dst _) -> do
  Just (srcPath, _) <- use (idxToFilepath . at src)
  Just (dstPath, _) <- use (idxToFilepath . at dst)
  putStrLn_ Debug $ srcPath ++ " >>> " ++ dstPath

buildBasicPlan ::
  (MonadState RenamerState m, MonadProc m, MonadLog m, MonadFail m) =>
  Maybe FilePath ->
  [RenamedFile] ->
  m [Mapping]
buildBasicPlan destDir = foldrM go []
  where
    go ren rest = do
      idx <-
        registerPath
          (target destDir ren)
          (ren ^. sourceDetails . checksum)
      pure $ Mapping (ren ^. sourceDetails . fileIdx) idx ren : rest

safeguardPlan ::
  (MonadState RenamerState m, MonadProc m, MonadLog m, MonadFail m) =>
  [Mapping] ->
  m [Mapping]
safeguardPlan plan = do
  pid <- getCurrentPid
  uncurry (++) <$> foldrM (work pid) ([], []) plan
  where
    (srcs, _dsts) = mappingSets plan
    work pid (Mapping src dst ren) (rest, post)
      | dst `S.member` srcs = do
          uniqueIdx <- nextUniqueNum
          Just (dstPath, csum) <- use (idxToFilepath . at dst)
          idx <-
            registerPath
              ( takeDirectory dstPath
                  </> "tmp_"
                  ++ show pid
                  ++ "_"
                  ++ show uniqueIdx
              )
              csum
          pure (Mapping src idx ren : rest, Mapping idx dst ren : post)
      | otherwise =
          pure (Mapping src dst ren : rest, post)

buildPlan ::
  ( MonadReader Options m,
    MonadState RenamerState m,
    MonadProc m,
    MonadLog m,
    MonadFail m
  ) =>
  Maybe FilePath ->
  [RenamedFile] ->
  m [Mapping]
buildPlan destDir rs = do
  reportIdempotentRenamings destDir rs
  reportOverlappedSources destDir rs
  reportOverlappedTargets destDir rs
  buildBasicPlan destDir rs >>= safeguardPlan

{-------------------------------------------------------------------------
 - Step 7: Execute
 -}

data Scenario = Scenario
  { _scenarioDetails :: [FileDetails],
    _scenarioSimpleRenamings :: [RenamedFile],
    _scenarioSiblingRenamings :: [RenamedFile],
    _scenarioRenamingsWithoutRedundancies :: [RenamedFile],
    -- | The final 'scenarioRenamings' include both simple and sibling
    --   renamings, and is clear of idempotent, redundant and overlapped
    --   renamings.
    _scenarioRenamings :: [RenamedFile],
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
    { _scenarioDetails = [],
      _scenarioSimpleRenamings = [],
      _scenarioSiblingRenamings = [],
      _scenarioRenamingsWithoutRedundancies = [],
      _scenarioRenamings = [],
      _scenarioMappings = []
    }

safeRemoveDirectory ::
  (MonadReader Options m, MonadLog m, MonadFS m) =>
  FilePath ->
  m ()
safeRemoveDirectory path = do
  putStrLn_ Normal $ "- " ++ path
  dry <- not <$> view execute
  unless dry $ removeDirectory path

safePruneDirectory ::
  (MonadReader Options m, MonadLog m, MonadFS m) =>
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
  (MonadReader Options m, MonadLog m, MonadFS m) =>
  FilePath ->
  m ()
safeRemoveFile path = do
  -- putStrLn_ Debug $ "- " ++ path
  dry <- not <$> view execute
  unless dry $ removeFile path

safeMoveFile ::
  ( MonadReader Options m,
    MonadState RenamerState m,
    MonadLog m,
    MonadFS m,
    MonadChecksum m
  ) =>
  (FilePath -> FilePath -> String) ->
  FilePath ->
  Maybe Checksum ->
  FilePath ->
  m ()
safeMoveFile label src srcSum dst
  | strToLower src == strToLower dst = do
      putStrLn_ Verbose $ src ++ " => " ++ dst
      dry <- not <$> view execute
      unless dry $ renameFile src dst
  | otherwise = do
      shouldMove <- case srcSum of
        Just csum ->
          preuse (fileChecksums . ix csum) >>= \case
            Just _ -> pure False
            _ -> pure True
        Nothing -> pure True
      if shouldMove
        then do
          putStrLn_ Verbose $ label src dst
          dry <- not <$> view execute
          unless dry $ do
            csum <- case srcSum of
              Just csum -> pure csum
              Nothing -> do
                isFile <- doesFileExist src
                -- This can be False if we are not in --execute mode, and the
                -- move is from a temp file that was never created.
                if isFile
                  then fileChecksum src
                  else pure ""
            isFile <- doesFileExist dst
            if isFile
              then do
                c <- view checksums
                if c
                  then do
                    csum' <- fileChecksum dst
                    if csum == csum'
                      then safeRemoveFile src
                      else
                        logErr $
                          "Destination already exists, cannot copy: "
                            ++ label src dst
                  else
                    logErr $
                      "Destination already exists, cannot copy: "
                        ++ label src dst
              else do
                copyFileWithMetadata src dst
                csum' <- fileChecksum dst
                if csum == csum'
                  then safeRemoveFile src
                  else logErr $ "Checksum mismatch: " ++ csum ++ " != " ++ csum'
        else do
          putStrLn_ Normal $ "INFO: Duplicate: " ++ src
          safeRemoveFile src

executePlan ::
  ( MonadReader Options m,
    MonadState RenamerState m,
    MonadLog m,
    MonadFS m,
    MonadChecksum m,
    MonadFail m
  ) =>
  TimeZone ->
  [Mapping] ->
  m ()
executePlan tz plan = do
  errors <- use errorCount
  if errors > 0
    then logErr "Cannot execute renaming plan with errors"
    else do
      putStrLn_ Normal $
        "Executing renaming plan ("
          ++ show (length plan)
          ++ " operations)..."
      forM_ plan $ \(Mapping src dst ren) -> do
        Just (srcPath, csum) <- use (idxToFilepath . at src)
        Just (dstPath, _) <- use (idxToFilepath . at dst)
        safeMoveFile (renamingLabel tz ren) srcPath csum dstPath
      putStrLn_ Normal "Renaming complete!"

type AppT m = ReaderT Options (StateT RenamerState m)

runAppT :: (Monad m) => Options -> AppT m a -> m a
runAppT opts k = evalStateT (runReaderT k opts) newRenamerState
