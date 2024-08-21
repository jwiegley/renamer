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
import Control.Lens hiding ((<.>))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Aeson hiding (Error, Options, decodeFileStrict, encodeFile, (.=))
import Data.Aeson qualified as JSON hiding (Error)
import Data.Char (toLower)
import Data.Foldable (foldrM, forM_)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap.Strict (IntMap)
import Data.IntSet qualified as S
import Data.List (group, nub, partition, sort, sortOn)
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
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FileDetails

type DetailsMap = HashMap FilePath FileDetails

data Renaming
  = SimpleRename UTCTime
  | SimpleRenameAvoidOverlap UTCTime
  | FollowTime FilePath
  | FollowBase FilePath
  deriving (Eq, Ord, Show)

makePrisms ''Renaming

data RenamedFile = RenamedFile
  { _sourceDetails :: FileDetails,
    _renamedFile :: FilePath,
    _renaming :: Renaming
  }
  deriving (Eq, Show)

makeLenses ''RenamedFile

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
  toEncoding = genericToEncoding defaultOptions

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
    _spanDirectories :: !Bool
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
      _spanDirectories = False
    }

class (Monad m) => MonadLog m where
  printLog :: String -> m ()

instance MonadLog IO where
  printLog = Pre.putStrLn

data Verbosity
  = Error
  | Normal
  | Verbose
  | Debug

putStrLn_ :: (MonadLog m) => Verbosity -> String -> AppT m ()
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
    $ lift
    $ lift
    $ printLog s

logErr :: (MonadLog m) => String -> AppT m ()
logErr msg = do
  putStrLn_ Error $ "ERROR: " ++ msg
  errorCount += 1

type AppT m = ReaderT Options (StateT RenamerState m)

runAppT :: (Monad m) => Options -> AppT m a -> m a
runAppT opts k = evalStateT (runReaderT k opts) newRenamerState

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
registerPath :: (Monad m) => FilePath -> Maybe Checksum -> AppT m Int
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

registerCounter :: (Monad m) => FilePath -> AppT m ()
registerCounter path = case path =~ nameRe of
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
        dailyCounter . at ymd ?= read counter
  _ -> pure ()

{-------------------------------------------------------------------------
 - Step 4: Analyze
 -}

class (Monad m) => MonadProc m where
  getCurrentPid :: m Pid

instance MonadProc IO where
  getCurrentPid = Proc.getCurrentPid

class (Monad m) => MonadParallel m where
  parallelInterleaved :: [m a] -> m [a]
  stopGlobalPool :: m ()

instance MonadParallel IO where
  parallelInterleaved = PIO.parallelInterleaved
  stopGlobalPool = PIO.stopGlobalPool

class (Monad m) => MonadChecksum m where
  fileChecksum :: FilePath -> m String

instance MonadChecksum IO where
  fileChecksum = b3sum

class (Monad m) => MonadPhoto m where
  photoCaptureDate :: FilePath -> m (Maybe UTCTime)

instance MonadPhoto IO where
  photoCaptureDate path =
    runMaybeT $
      exiftoolSubSecDateTimeOriginal path
        <|> exiftoolDateTimeOriginal path
        <|> exiv2ImageTimestamp path

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

class (Monad m) => MonadJSON m where
  decodeFileStrict :: (FromJSON a) => FilePath -> m (Maybe a)
  encodeFile :: (ToJSON a) => FilePath -> a -> m ()

instance MonadJSON IO where
  decodeFileStrict = JSON.decodeFileStrict
  encodeFile = JSON.encodeFile

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

registerFileDetails :: (Monad m) => Bool -> FileDetails -> AppT m FileDetails
registerFileDetails gather FileDetails {..} = do
  when gather $
    registerCounter _filepath
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
        then do
          concatMapM (walkFileEntries recurse f . (path </>))
            =<< listDirectory path
        else map (f . (path </>)) <$> listDirectory path
    else pure [f path]

-- | Takes a list of files and/or directories, and gathers file details for
--   all files involved, recursively.
gatherDetails ::
  ( MonadLog m,
    MonadFS m,
    MonadJSON m,
    MonadParallel m,
    MonadChecksum m,
    MonadPhoto m,
    Alternative m
  ) =>
  Bool ->
  [FilePath] ->
  AppT m [FileDetails]
gatherDetails gather = concatMapM $ \entry -> do
  -- Get info on all entries; this is stateful and builds up the following
  -- tables:
  --   filepathToIdx
  --   idxToFilepath
  --   fileChecksums
  c <- view checksums
  recurse <- view recursive
  isDir <- lift $ lift $ doesDirectoryExist entry
  details <-
    if isDir
      then do
        let detailsFile = entry </> ".file-details.json"
        isFile <- lift $ lift $ doesFileExist detailsFile
        stateful <- view keepState
        mres <-
          if stateful && isFile
            then lift $ lift $ decodeFileStrict detailsFile
            else pure Nothing
        case mres of
          Just (details, st) -> do
            lift $ put st
            pure details
          Nothing -> do
            putStrLn_ Verbose $ "Gathering details from " ++ show entry
            details <-
              lift $
                lift $
                  parallelInterleaved
                    =<< walkFileEntries recurse (getFileDetails c) entry
            st <- lift get
            lift $ lift $ encodeFile detailsFile (details, st)
            pure details
      else do
        putStrLn_ Verbose $ "Gathering details from " ++ show entry
        lift $
          lift $
            parallelInterleaved
              =<< walkFileEntries recurse (getFileDetails c) entry
  lift $ lift stopGlobalPool
  mapM (registerFileDetails gather) details

{-------------------------------------------------------------------------
 - Step 5: Naming
 -}

yymmdd :: LocalTime -> String
yymmdd = formatTime defaultTimeLocale "%y%m%d"

nextSeqNum :: (Monad m) => String -> AppT m Int
nextSeqNum ymd =
  zoom dailyCounter $
    preuse (ix ymd) >>= \case
      Just idx -> idx <$ (ix ymd += 1)
      Nothing -> 1 <$ (at ymd ?= 2)

nextUniqueNum :: (Monad m) => AppT m Int
nextUniqueNum = do
  uniqueIdx <- use uniqueCounter
  uniqueCounter += 1
  pure uniqueIdx

normalizeExt :: String -> String
normalizeExt ext = case strToLower ext of
  ".jpeg" -> ".jpg"
  ".tiff" -> ".tif"
  ext' -> ext'

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
  (Monad m) =>
  TimeZone ->
  [FileDetails] ->
  AppT m [RenamedFile]
simpleRenamings tz =
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
            work ren base details = (RenamedFile details named ren :)
              where
                named = name details
                name d = base <.> ext d
                ext d = normalizeExt (d ^. fileext)

            expectedBase = do
              spanDirs <- view spanDirectories
              newName (yymmdd tm')
                <$> nextSeqNum
                  ( ( if spanDirs
                        then id
                        else (e ^. filedir </>)
                    )
                      (yymmdd tm')
                  )
              where
                tm' = utcToLocalTime tz tm
                newName base seqNum = base ++ "_" ++ printf "%04d" seqNum

-- | Entries that would rename a file to itself.
idempotentRenaming :: Maybe FilePath -> RenamedFile -> Bool
idempotentRenaming destDir ren = ren ^. source == target destDir ren

reportIdempotentRenamings ::
  (MonadLog m) =>
  Maybe FilePath ->
  [RenamedFile] ->
  AppT m ()
reportIdempotentRenamings destDir rs =
  forM_ (filter (idempotentRenaming destDir) rs) $ \ren ->
    logErr $
      "Renaming file to itself: " ++ show ren

redundantRenaming :: Maybe FilePath -> RenamedFile -> RenamedFile -> Bool
redundantRenaming destDir rx ry =
  rx ^. source == ry ^. source && target destDir rx == target destDir ry

removeRedundantSourceRenamings ::
  Maybe FilePath -> [RenamedFile] -> [RenamedFile]
removeRedundantSourceRenamings destDir rs =
  Prelude.map
    NE.head
    (NE.groupBy (redundantRenaming destDir) (sortOn (^. source) rs))

removeRedundantTargetRenamings ::
  Maybe FilePath -> [RenamedFile] -> [RenamedFile]
removeRedundantTargetRenamings destDir rs =
  Prelude.map
    NE.head
    (NE.groupBy (redundantRenaming destDir) (sortOn (target destDir) rs))

overlapped ::
  (RenamedFile -> FilePath) ->
  [RenamedFile] ->
  [(FilePath, [RenamedFile])]
overlapped f rs = do
  nm <- duplicatedElements (Prelude.map f rs)
  pure (nm, filter ((== nm) . f) rs)

reportOverlappedSources ::
  (MonadLog m) =>
  Maybe FilePath ->
  [RenamedFile] ->
  AppT m ()
reportOverlappedSources destDir rs =
  forM_ (overlapped (^. source) rs) $ \(src, dsts) ->
    forM_ dsts $ \dst ->
      logErr $ "Overlapped source: " ++ src ++ " -> " ++ target destDir dst

reportOverlappedTargets ::
  (MonadLog m) =>
  Maybe FilePath ->
  [RenamedFile] ->
  AppT m ()
reportOverlappedTargets destDir rs =
  forM_ (overlapped (target destDir) rs) $ \(dst, srcs) ->
    forM_ srcs $ \src ->
      logErr $ "Overlapped target: " ++ src ^. source ++ " -> " ++ dst

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
  (Monad m) =>
  TimeZone ->
  Maybe FilePath ->
  [FileDetails] ->
  ([RenamedFile] -> AppT m [RenamedFile]) ->
  ([RenamedFile] -> AppT m [RenamedFile]) ->
  ([RenamedFile] -> AppT m [RenamedFile]) ->
  ([RenamedFile] -> AppT m [RenamedFile]) ->
  AppT m [RenamedFile]
renameFiles tz destDir ds k1 k2 k3 k4 = do
  rs1 <- k1 =<< simpleRenamings tz ds
  let rs1' = filter (not . idempotentRenaming destDir) rs1
  rs2 <- k2 (siblingRenamings ds rs1')
  rs3 <- k3 (rs1' ++ rs2)
  k4
    ( removeRedundantTargetRenamings destDir $
        removeRedundantSourceRenamings destDir $
          filter (not . idempotentRenaming destDir) $
            rs3
    )

{-------------------------------------------------------------------------
 - Step 6: Plan
 -}

hasUniqueExts :: [FileDetails] -> Bool
hasUniqueExts =
  null . duplicatedElements . map (normalizeExt . (^. fileext))

buildPlan ::
  (MonadProc m, MonadLog m, MonadFail m) =>
  Maybe FilePath ->
  [RenamedFile] ->
  AppT m [(Int, Int, RenamedFile)]
buildPlan destDir rs = do
  reportIdempotentRenamings destDir rs
  reportOverlappedSources destDir rs
  reportOverlappedTargets destDir rs

  pid <- lift $ lift $ getCurrentPid
  rs' <- foldrM go [] rs
  safeguardPlan pid rs'
  where
    go ren rest = do
      idx <-
        registerPath
          (target destDir ren)
          (ren ^. sourceDetails . checksum)
      pure $ (ren ^. sourceDetails . fileIdx, idx, ren) : rest

    safeguardPlan pid plan = uncurry (++) <$> foldrM work ([], []) plan
      where
        (srcs, _dsts, _) = unzip3 plan
        srcs' = S.fromList srcs
        work (src, dst, ren) (rest, post)
          | dst `S.member` srcs' = do
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
              pure ((src, idx, ren) : rest, (idx, dst, ren) : post)
          | otherwise =
              pure ((src, dst, ren) : rest, post)

{-------------------------------------------------------------------------
 - Step 7: Execute
 -}

safeRemoveDirectory :: (MonadLog m, MonadFS m) => FilePath -> AppT m ()
safeRemoveDirectory path = do
  putStrLn_ Normal $ "- " ++ path
  dry <- not <$> view execute
  unless dry $ lift $ lift $ removeDirectory path

safePruneDirectory :: (MonadLog m, MonadFS m) => FilePath -> AppT m ()
safePruneDirectory path = do
  entries <- lift $ lift $ listDirectory path
  safeToRemove <- flip execStateT True $
    forM_ entries $ \entry -> do
      let entryPath = path </> entry
      isDir <- lift $ lift $ lift $ doesDirectoryExist entryPath
      if isDir
        then lift $ safePruneDirectory entryPath
        else put False
  when safeToRemove $
    safeRemoveDirectory path

safeRemoveFile :: (MonadLog m, MonadFS m) => FilePath -> AppT m ()
safeRemoveFile path = do
  putStrLn_ Debug $ "- " ++ path
  dry <- not <$> view execute
  unless dry $ lift $ lift $ removeFile path

safeMoveFile ::
  (MonadLog m, MonadFS m, MonadChecksum m) =>
  String ->
  FilePath ->
  Maybe Checksum ->
  FilePath ->
  AppT m ()
safeMoveFile label src srcSum dst
  | strToLower src == strToLower dst = do
      putStrLn_ Verbose $ src ++ " => " ++ dst
      dry <- not <$> view execute
      unless dry $ lift $ lift $ renameFile src dst
  | otherwise = do
      shouldMove <- case srcSum of
        Just csum ->
          preuse (fileChecksums . ix csum) >>= \case
            Just _ -> pure False
            _ -> pure True
        Nothing -> pure True
      if shouldMove
        then do
          putStrLn_ Normal $ src ++ label ++ dst
          dry <- not <$> view execute
          unless dry $ do
            csum <- case srcSum of
              Just csum -> pure csum
              Nothing -> do
                isFile <- lift $ lift $ doesFileExist src
                -- This can be False if we are not in --execute mode, and the
                -- move is from a temp file that was never created.
                if isFile
                  then lift $ lift $ fileChecksum src
                  else pure ""
            isFile <- lift $ lift $ doesFileExist dst
            if isFile
              then do
                csum' <- lift $ lift $ fileChecksum dst
                if csum == csum'
                  then safeRemoveFile src
                  else logErr $ "Destination already exists: " ++ dst
              else do
                lift $ lift $ copyFileWithMetadata src dst
                csum' <- lift $ lift $ fileChecksum dst
                if csum == csum'
                  then safeRemoveFile src
                  else logErr $ "Checksum mismatch: " ++ csum ++ " != " ++ csum'
        else do
          putStrLn_ Normal $ "INFO: Duplicate: " ++ src
          safeRemoveFile src

executePlan ::
  (MonadLog m, MonadFS m, MonadChecksum m, MonadFail m) =>
  TimeZone ->
  [(Int, Int, RenamedFile)] ->
  AppT m ()
executePlan tz plan = do
  errors <- use errorCount
  if errors > 0
    then logErr "Cannot execute renaming plan with errors"
    else do
      putStrLn_ Verbose $
        "Executing renaming plan ("
          ++ show (length plan)
          ++ " operations)..."
      forM_ plan $ \(src, dst, ren) -> do
        Just (srcPath, csum) <- use (idxToFilepath . at src)
        Just (dstPath, _) <- use (idxToFilepath . at dst)
        safeMoveFile (label (ren ^. renaming)) srcPath csum dstPath
      putStrLn_ Normal "Renaming complete!"
  where
    formattedTime tm =
      formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" tm'
      where
        tm' = utcToLocalTime tz tm

    label = \case
      SimpleRename tm -> " (" ++ formattedTime tm ++ ")-> "
      SimpleRenameAvoidOverlap tm -> " (" ++ formattedTime tm ++ ")!> "
      FollowBase name -> " [" ++ name ++ "]-> "
      FollowTime name -> " {" ++ name ++ "}-> "
