{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Renamer
  ( Command (..),
    MonadJSON (..),
    MonadLog (..),
    MonadProc (..),
    MonadParallel (..),
    MonadPhoto (..),
    MonadFSRead (..),
    MonadFSWrite (..),
    Reason (..),
    Renaming (Renaming),
    HasRenaming (..),
    Mapping,
    Scenario (..),
    Prefix (Prefix),
    HasScenario (..),
    PhotoGroup (PhotoGroup),
    FileDetails,
    HasFileDetails (..),
    Options (Options),
    HasOptions (..),
    defaultOptions,
    Verbosity (..),
    putStrLn_,
    whenDebug,
    scenarioDetails,
    determineScenario,
    renamerExecute,
    buildPlan,
    executePlan,
    groupPhotos,
    computeRenamings,
    cleanRenamings,
    gatherDetails,
    renderMappings,
    renderDetails,
    processDetails,
    groupRenamingsBy,
    idempotentRenaming,
    removeRedundantRenamings,
    safePruneDirectory,
    AppT,
    runAppT,
  )
where

import Control.Applicative
import Control.Concurrent.ParallelIO qualified as PIO
import Control.Exception (assert)
import Control.Lens hiding ((<.>))
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Aeson hiding
  ( Error,
    Options,
    decodeFileStrict,
    defaultOptions,
    encodeFile,
    (.=),
  )
import Data.Aeson qualified as JSON hiding (Error)
import Data.Char (toLower)
import Data.Foldable (find, foldrM, forM_)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntSet (IntSet)
import Data.List (group, nub, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Time
import Data.Traversable (forM)
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import System.Directory qualified as Dir
import System.Exit
import System.FilePath
import System.IO (hFlush, stdout)
import System.Process (Pid, readProcessWithExitCode)
import System.Process qualified as Proc
import Text.Printf
import Text.Regex.TDFA hiding (after, before)
import Text.Regex.TDFA.String ()
import Text.Show.Pretty (ppShow)
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

makeClassy ''FileDetails

instance ToJSON FileDetails where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON FileDetails

-- | State of the image repository (new or old).
data RenamerState = RenamerState
  { _nameCounter :: HashMap String Int,
    _nameReservations :: HashMap String IntSet,
    -- | Mapping from file basename to list of entries sharing that basename,
    --   and whatever renaming has been determined for that base
    _entriesAtBase :: HashMap FilePath [FileDetails],
    _renamedEntries :: HashSet FilePath,
    -- | A unique counter used to name temporary files
    _uniqueCounter :: Integer,
    _errorCount :: Integer
  }
  deriving (Show, Generic)

makeClassy ''RenamerState

instance ToJSON RenamerState where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON RenamerState

newRenamerState :: RenamerState
newRenamerState =
  RenamerState
    { _nameCounter = mempty,
      _nameReservations = mempty,
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

makeClassy ''Options

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
  printLog str = do
    Pre.putStrLn str
    hFlush stdout

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

logWarn ::
  ( MonadReader Options m,
    MonadLog m
  ) =>
  String ->
  m ()
logWarn msg = putStrLn_ Normal $ "WARNING: " ++ msg

{-------------------------------------------------------------------------
 - Step 3: Database manipulation
 -}

newtype Prefix = Prefix {getPrefix :: FilePath}
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Prefix where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Prefix

fileroot :: Bool -> Lens' FileDetails FilePath
fileroot spanDirs f s
  | spanDirs =
      let ext = s ^. fileext
       in ( \path ->
              s
                & filepath .~ s ^. filedir </> path <.> ext
                & filename .~ path <.> ext
                & filebase .~ path
          )
            <$> f (s ^. filebase)
  | otherwise =
      let ext = s ^. fileext
       in ( \path ->
              s
                & filepath .~ path <.> ext
                & filedir .~ takeDirectory path
                & filename .~ takeFileName path <.> ext
                & filebase .~ takeBaseName path
          )
            <$> f (dropExtension (s ^. filepath))

yymmdd :: LocalTime -> String
yymmdd = formatTime defaultTimeLocale "%y%m%d"

normalizeExt :: String -> String
normalizeExt ext = case strToLower ext of
  ".jpeg" -> ".jpg"
  ".tiff" -> ".tif"
  ext' -> ext'

-- Given a directory /foo/bar, this calculates what the "key" directory is for
-- calculations related to the counter.
keyDir :: Bool -> Maybe FilePath -> FilePath -> FilePath
keyDir True _ _ = ""
keyDir _spanDirs@False Nothing dir = dir
keyDir _spanDirs@False _destDir@(Just dir) _ = dir

goalPath ::
  Maybe FilePath ->
  FileDetails ->
  Prefix ->
  Int ->
  FilePath
goalPath destDir details (Prefix prefix) num =
  fromMaybe (details ^. filedir) destDir
    </> prefix
    ++ "_"
    ++ printf "%04d" num
      <.> normalizeExt (details ^. fileext)

nextNameCounter ::
  (MonadReader Options m, MonadState RenamerState m) =>
  Prefix ->
  m Int
nextNameCounter (Prefix prefix) = do
  spanDirs <- view spanDirectories
  preuse (nameCounter . ix prefix) >>= \case
    Just idx -> go spanDirs idx
    Nothing -> do
      nameCounter . at prefix ?= 1
      go spanDirs 1
  where
    go spanDirs i = do
      nameCounter . ix prefix += 1
      preuse (nameReservations . ix prefix) >>= \case
        Just s | has (ix i) s -> go spanDirs (succ i)
        _ -> pure i

nameRe :: String
nameRe = "^([0-9][0-9][0-9][0-9][0-9][0-9])_([0-9][0-9][0-9][0-9])$"

maybeWithCounter :: (Monad m) => FilePath -> (String -> Int -> m ()) -> m ()
maybeWithCounter path f = case takeBaseName path =~ nameRe of
  [(_ : prefix : counter : [])] ->
    forM_
      ( parseTimeM
          False
          defaultTimeLocale
          "%0y%0m%0d"
          prefix ::
          Maybe UTCTime
      )
      $ \_ -> f prefix (read counter)
  _ -> pure ()

maybeUpdateNameCounter ::
  (MonadReader Options m, MonadState RenamerState m) =>
  Maybe FilePath ->
  FilePath ->
  m ()
maybeUpdateNameCounter destDir path =
  maybeWithCounter path $ \prefix counter -> do
    spanDirs <- view spanDirectories
    nameCounter . at (keyDir spanDirs destDir (takeDirectory path) </> prefix)
      %= Just . \case
        Just count -> max count (succ counter)
        Nothing -> succ counter

maybeReserveCounter ::
  (MonadReader Options m, MonadState RenamerState m) =>
  Maybe FilePath ->
  FilePath ->
  m ()
maybeReserveCounter destDir path =
  maybeWithCounter path $ \prefix counter -> do
    spanDirs <- view spanDirectories
    nameReservations
      . at (keyDir spanDirs destDir (takeDirectory path) </> prefix)
      %= Just . \case
        Just s -> s & at counter ?~ ()
        Nothing -> mempty & at counter ?~ ()

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

renderDetails ::
  (MonadReader Options m, MonadLog m) =>
  [FileDetails] ->
  m ()
renderDetails = mapM_ $ \d ->
  putStrLn_ Debug $
    d ^. filepath ++ maybe "" ((" @ " ++) . show) (d ^. captureTime)

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

registerFileDetails ::
  (MonadReader Options m, MonadState RenamerState m) =>
  Maybe FilePath ->
  FileDetails ->
  m FileDetails
registerFileDetails destDir FileDetails {..} = do
  when (isJust destDir) $
    maybeUpdateNameCounter destDir _filepath
  pure FileDetails {..}

processDetails ::
  (MonadReader Options m, MonadState RenamerState m) =>
  Maybe FilePath ->
  [FileDetails] ->
  m [FileDetails]
processDetails mdest = mapM (registerFileDetails mdest) . sort

{-------------------------------------------------------------------------
 - Step 5: Naming
 -}

-- Why is this file in the photo group?
data Reason
  = ForTime UTCTime
  | ForBase FilePath
  deriving (Eq, Ord, Show, Generic)

makePrisms ''Reason

instance ToJSON Reason where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Reason

-- A photo group is a set of files that will all receive the same name minus
-- extension, and so all entries must have different normalized extensions.
data PhotoGroup = PhotoGroup (NonEmpty (FileDetails, Reason)) Prefix
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PhotoGroup where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON PhotoGroup

data Renaming a b = Renaming
  { _renamingFrom :: a,
    _renamingTo :: b,
    _renamingFor :: Reason
  }
  deriving (Eq, Show, Generic)

makeClassy ''Renaming

instance (ToJSON a, ToJSON b) => ToJSON (Renaming a b) where
  toEncoding = genericToEncoding JSON.defaultOptions

instance (FromJSON a, FromJSON b) => FromJSON (Renaming a b)

hasUniqueExts :: [FileDetails] -> Bool
hasUniqueExts = null . duplicatedElements . map (normalizeExt . (^. fileext))

prefixFromTime :: FilePath -> TimeZone -> UTCTime -> Prefix
prefixFromTime path tz tm = Prefix (path </> yymmdd (utcToLocalTime tz tm))

-- Even if two files have the same base name, if they have capture times that
-- differ, we do not group them together.
groupDetailsByTime :: Bool -> [FileDetails] -> [NonEmpty FileDetails]
groupDetailsByTime spanDirs = NE.groupBy $ \x y ->
  (spanDirs || x ^. filedir == y ^. filedir)
    && case (x ^. captureTime, y ^. captureTime) of
      (Just tmx, Just tmy) -> tmx == tmy
      _ -> False

-- If a group has the same time, or the same base, and there is more than one
-- of each file extension, then we cannot be certain and must split up that
-- group, reporting this fact to the user.
keepGroupsIf :: (NonEmpty a -> Bool) -> [NonEmpty a] -> [NonEmpty a]
keepGroupsIf f = foldr go []
  where
    go xs rest
      | f xs = xs : rest
      | otherwise = NE.toList (NE.map (:| []) xs) ++ rest

gathering :: (a -> a -> Bool) -> [NonEmpty a] -> [NonEmpty a]
gathering f = foldr go []
  where
    go xs@(x :| []) rest =
      case after of
        [] -> xs : rest
        (y : ys) -> before ++ NE.append y xs : ys
      where
        (before, after) =
          break (\ys -> isJust (find (f x) ys)) rest
    go xs rest = xs : rest

gatherRoots :: Bool -> [NonEmpty FileDetails] -> [NonEmpty FileDetails]
gatherRoots spanDirs = gathering $ \x y ->
  isNothing (x ^. captureTime)
    && isJust (y ^. captureTime)
    && x ^. fileroot spanDirs == y ^. fileroot spanDirs

-- This is the most complex function in the renamer, since it's job is to turn
-- a set of file details into an identified set of files and photo groups.
groupPhotos ::
  Bool ->
  Maybe FilePath ->
  TimeZone ->
  [FileDetails] ->
  [Either FileDetails PhotoGroup]
groupPhotos spanDirs destDir tz =
  Prelude.map go
    . gatherRoots spanDirs
    . keepGroupsIf (hasUniqueExts . NE.toList)
    . groupDetailsByTime spanDirs
  where
    key = keyDir spanDirs destDir

    go (d :| []) = case d ^. captureTime of
      Just tm ->
        Right $
          PhotoGroup
            ((d, ForTime tm) :| [])
            (prefixFromTime (key (d ^. filedir)) tz tm)
      Nothing ->
        Left d
    go ds =
      Right
        ( PhotoGroup
            (NE.map (\d -> (d, reason d)) ds)
            (prefixFromTime (key getDir) tz getTime)
        )
      where
        reason d =
          maybe
            (ForBase (d ^. fileroot spanDirs))
            ForTime
            (d ^. captureTime)

        times = catMaybes (NE.toList (NE.map (^. captureTime) ds))

        getTime = case nub times of
          [tm] -> tm
          _ -> error "computeRenamings: unexpected times"

        dirs = NE.toList (NE.map (^. filedir) ds)

        getDir = case nub dirs of
          [dir] -> dir
          _ -> error "computeRenamings: unexpected directories"

computeRenamings ::
  (MonadReader Options m, MonadState RenamerState m) =>
  Maybe FilePath ->
  [Either FileDetails PhotoGroup] ->
  m [Mapping]
computeRenamings destDir = concatMapM go . sort
  where
    go (Left x) = [] <$ maybeReserveCounter destDir (x ^. filepath)
    go (Right (PhotoGroup xs (Prefix prefix))) = do
      num <- nextNameCounter (Prefix prefix)
      pure $
        NE.toList $
          NE.map
            ( \(x, r) ->
                Renaming
                  (x ^. filepath)
                  (goalPath destDir x (Prefix (takeBaseName prefix)) num)
                  r
            )
            xs

mappingLabel :: TimeZone -> Renaming FilePath FilePath -> String
mappingLabel tz ren =
  ren ^. renamingFrom
    ++ case ren ^. renamingFor of
      ForTime tm -> " (" ++ formattedTime tm ++ ")-> "
      ForBase name -> " [" ++ name ++ "]-> "
    ++ ren ^. renamingTo
  where
    formattedTime tm =
      formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" tm'
      where
        tm' = utcToLocalTime tz tm

renderMappings ::
  (MonadReader Options m, MonadLog m) =>
  TimeZone ->
  [Mapping] ->
  m ()
renderMappings tz = mapM_ $ \r ->
  putStrLn_ Debug $ mappingLabel tz r

-- | Entries that would rename a file to itself.
idempotentRenaming :: Mapping -> Bool
idempotentRenaming ren = ren ^. renamingFrom == ren ^. renamingTo

reportIdempotentRenamings ::
  (MonadReader Options m, MonadState RenamerState m, MonadLog m) =>
  TimeZone ->
  [Mapping] ->
  m ()
reportIdempotentRenamings tz rs =
  forM_ (filter idempotentRenaming rs) $ \ren ->
    logErr $
      "Renaming file to itself: " ++ mappingLabel tz ren

redundantRenaming ::
  Mapping ->
  Mapping ->
  Bool
redundantRenaming rx ry =
  rx ^. renamingFrom == ry ^. renamingFrom
    && rx ^. renamingTo == ry ^. renamingTo

removeRedundantRenamings ::
  (Mapping -> FilePath) ->
  [Mapping] ->
  [Mapping]
removeRedundantRenamings f =
  -- Prelude.map NE.head . sortAndGroupOn f
  Prelude.map NE.head . NE.groupBy redundantRenaming . sortOn f

groupRenamingsBy ::
  (Mapping -> FilePath) ->
  [Mapping] ->
  [NonEmpty (Mapping)]
groupRenamingsBy f = filter (\xs -> NE.length xs > 1) . sortAndGroupOn f

removeOverlappedRenamings ::
  [Mapping] ->
  ( [Mapping],
    [(Mapping, NonEmpty (Mapping))]
  )
removeOverlappedRenamings rs =
  ( Prelude.map fst rs'',
    onlyOverlaps rs' ++ onlyOverlaps rs''
  )
  where
    rs' = nonOverlapped (^. renamingFrom) rs
    rs'' = nonOverlapped (^. renamingTo) (Prelude.map fst rs')

    nonOverlapped f =
      foldr
        ( \rens rest ->
            let k r = case r ^. renamingFor of
                  ForBase _ ->
                    r ^. renamingFrom
                      `notElem` rest ^.. traverse . _1 . renamingFor . _ForBase
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
                case sortOn (^. renamingFor) (NE.filter k rens) of
                  [] -> error "Unexpected: removeOverlappedRenamings"
                  y : ys -> (y, ys) : rest
        )
        []
        . sortAndGroupOn f

    onlyOverlaps = concatMap $ \(x, xs) -> case xs of
      [] -> []
      y : ys -> [(x, y :| ys)]

reportOverlappedSources ::
  (MonadReader Options m, MonadState RenamerState m, MonadLog m) =>
  TimeZone ->
  [Mapping] ->
  m ()
reportOverlappedSources tz rs =
  forM_ (groupRenamingsBy (^. renamingFrom) rs) $ \dsts ->
    forM_ dsts $ \dst ->
      logErr $
        "Overlapped source: " ++ mappingLabel tz dst

reportOverlappedTargets ::
  (MonadReader Options m, MonadState RenamerState m, MonadLog m) =>
  TimeZone ->
  [Mapping] ->
  m ()
reportOverlappedTargets tz rs =
  forM_ (groupRenamingsBy (^. renamingTo) rs) $ \srcs ->
    forM_ srcs $ \src ->
      logErr $
        "Overlapped target: " ++ mappingLabel tz src

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
cleanRenamings ::
  (MonadReader Options m, MonadState RenamerState m, MonadLog m) =>
  TimeZone ->
  [Mapping] ->
  m [Mapping]
cleanRenamings tz rs =
  assert
    ( Prelude.all
        (\g -> length (NE.filter (has (renamingFor . _ForTime)) g) < 2)
        (groupRenamingsBy (^. renamingFrom) rs)
    )
    $ do
      let rs' =
            removeRedundantRenamings (^. renamingTo) $
              removeRedundantRenamings (^. renamingFrom) $
                filter (not . idempotentRenaming) $
                  rs
          (rs'', overlaps) = removeOverlappedRenamings rs'
      forM_ overlaps $ \(x, ys) -> do
        putStrLn_ Normal $ "Preferring this renaming:"
        putStrLn_ Normal $ "    " ++ mappingLabel tz x
        putStrLn_ Normal $ "  over these:"
        forM_ ys $ \y ->
          putStrLn_ Normal $ "    " ++ mappingLabel tz y
      pure rs''

{-------------------------------------------------------------------------
 - Step 6: Plan
 -}

type Mapping = Renaming FilePath FilePath

nextUniqueNum :: (MonadState RenamerState m) => m Integer
nextUniqueNum = do
  uniqueIdx <- use uniqueCounter
  uniqueCounter += 1
  pure uniqueIdx

buildPlan ::
  ( MonadReader Options m,
    MonadState RenamerState m,
    MonadProc m,
    MonadLog m
  ) =>
  TimeZone ->
  [Mapping] ->
  m [Mapping]
buildPlan tz plan = do
  reportIdempotentRenamings tz plan
  reportOverlappedSources tz plan
  reportOverlappedTargets tz plan

  pid <- getCurrentPid
  (_, xs, ys) <- foldrM (work pid) (mempty :: HashSet FilePath, [], []) plan
  pure $ xs ++ ys
  where
    work pid (Renaming src dst ren) (srcs, rest, post)
      | has (ix dst) srcs = do
          uniqueIdx <- nextUniqueNum
          let tmp =
                takeDirectory dst
                  </> "tmp_"
                  ++ show pid
                  ++ "_"
                  ++ show uniqueIdx
          pure
            ( srcs & at src ?~ (),
              Renaming src tmp ren : rest,
              Renaming tmp dst ren : post
            )
      | otherwise =
          pure
            ( srcs & at src ?~ (),
              Renaming src dst ren : rest,
              post
            )

{-------------------------------------------------------------------------
 - Step 7: Execute
 -}

data Scenario = Scenario
  { _scenarioPid :: Int,
    _scenarioTimeZoneMinutes :: Int,
    _scenarioRepository :: [FileDetails],
    _scenarioDestination :: Maybe FilePath,
    _scenarioInputs :: [FileDetails],
    _scenarioPhotoGroups :: [Either FileDetails PhotoGroup],
    _scenarioSimpleRenamings :: [Mapping],
    _scenarioRenamings :: [Mapping],
    _scenarioMappings :: [Mapping]
  }
  deriving (Eq, Show, Generic)

makeClassy ''Scenario

instance ToJSON Scenario where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Scenario

scenarioDetails ::
  ( MonadReader Options m,
    MonadState RenamerState m,
    MonadProc m,
    MonadFSRead m,
    MonadJSON m,
    MonadParallel m,
    MonadPhoto m,
    MonadLog m
  ) =>
  [FilePath] ->
  [FilePath] ->
  Maybe FilePath ->
  m ([FileDetails], [FileDetails])
scenarioDetails repos inputs destDir = do
  putStrLn_ Normal "Gathering details..."
  rds <-
    if null repos
      then pure []
      else
        gatherDetails repos
          >>= processDetails destDir
  whenDebug $ renderDetails rds
  ds <-
    gatherDetails inputs
      >>= processDetails destDir
  whenDebug $ renderDetails ds
  pure (rds, ds)

determineScenario ::
  ( MonadReader Options m,
    MonadState RenamerState m,
    MonadProc m,
    MonadFSRead m,
    MonadJSON m,
    MonadParallel m,
    MonadPhoto m,
    MonadLog m
  ) =>
  TimeZone ->
  [FileDetails] ->
  [FileDetails] ->
  Maybe FilePath ->
  m Scenario
determineScenario
  tz
  _scenarioRepository
  _scenarioInputs
  _scenarioDestination = do
    _scenarioPid <- fromIntegral <$> getCurrentPid
    let _scenarioTimeZoneMinutes = timeZoneMinutes tz
    (_scenarioPhotoGroups, _scenarioSimpleRenamings, _scenarioRenamings) <-
      doRenameFiles
        ( if null _scenarioInputs
            then _scenarioRepository
            else _scenarioInputs
        )
    _scenarioMappings <- doBuildPlan _scenarioRenamings
    pure Scenario {..}
    where
      doRenameFiles details = do
        putStrLn_ Normal $
          "Determining expected file names (from "
            ++ show (length details)
            ++ " entries)..."

        spanDirs <- view spanDirectories
        let gs = groupPhotos spanDirs _scenarioDestination utc details
        whenDebug $ forM_ gs $ putStrLn_ Debug . ppShow
        srs <- computeRenamings _scenarioDestination gs
        whenDebug $ renderMappings tz srs
        rs <- cleanRenamings tz srs
        whenDebug $ renderMappings tz rs
        pure (gs, srs, rs)

      doBuildPlan renamings = do
        putStrLn_ Normal $
          "Building renaming plan (from "
            ++ show (length renamings)
            ++ " renamings)..."
        p <- buildPlan tz renamings
        whenDebug $ renderMappings tz p
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
  String ->
  FilePath ->
  FilePath ->
  m Integer
safeMoveFile label src dst
  | strToLower src == strToLower dst = do
      putStrLn_ Verbose $ src ++ " => " ++ dst
      renameFile src dst
      pure 0
  | otherwise = do
      putStrLn_ Verbose label
      isFile <- doesFileExist dst
      if isFile
        then do
          logWarn $
            "Destination already exists, appending + suffix: "
              ++ label
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
  results <- forM plan $ \ren@(Renaming src dst _) ->
    safeMoveFile (mappingLabel tz ren) src dst
  let errors = sum results
  putStrLn_ Normal $ "Renaming completed with " ++ show errors ++ " errors"
  pure errors

renamerExecute ::
  ( MonadReader Options m,
    MonadState RenamerState m,
    MonadLog m,
    MonadFSRead m,
    MonadFSWrite m
  ) =>
  TimeZone ->
  Scenario ->
  m Integer
renamerExecute tz s = do
  errors <- use errorCount
  if errors > 0
    then errors <$ logErr "Cannot execute renaming plan with errors"
    else executePlan tz (s ^. scenarioMappings)

type AppT m = ReaderT Options (StateT RenamerState m)

runAppT :: (Monad m) => Options -> AppT m a -> m a
runAppT opts k = evalStateT (runReaderT k opts) newRenamerState
