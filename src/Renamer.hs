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
    SimpleMapping,
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
    strToLower,
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
import Control.Arrow ((&&&))
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
import Data.Foldable (Foldable (foldr'), find, foldrM, forM_)
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
{-# INLINE strToLower #-}

concatMapM :: (Traversable t, Monad m) => (a -> m [b]) -> t a -> m [b]
concatMapM = (fmap concat .) . mapM
{-# INLINE concatMapM #-}

duplicatedElements :: (Ord a) => [a] -> [a]
duplicatedElements =
  nub . concat . filter ((> 1) . length) . group . sort
{-# INLINE duplicatedElements #-}

sortAndGroupOn :: (Ord b) => (a -> b) -> [a] -> [NonEmpty a]
sortAndGroupOn f = NE.groupBy ((==) `on` f) . sortOn f
{-# INLINE sortAndGroupOn #-}

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
  { _filedir :: FilePath, -- "/foo"
    _captureTime :: Maybe UTCTime,
    _filepath :: FilePath, -- "/foo/bar.CR3"
    _filename :: FilePath, -- "bar.CR3"
    _filebase :: FilePath, -- "bar"
    _fileroot :: FilePath, -- "/foo/bar"
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
    _extraDebug :: !Bool,
    _jobs :: !Int,
    _recursive :: !Bool,
    _execute :: !Bool,
    _keepState :: !Bool,
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
      _extraDebug = False,
      _jobs = 1,
      _recursive = False,
      _execute = False,
      _keepState = False,
      _scenarioTo = Nothing,
      _scenarioFrom = Nothing
    }

class (Monad m) => MonadLog m where
  printLog :: String -> m ()
  flushLog :: m ()

instance MonadLog IO where
  flushLog = hFlush stdout
  printLog str = Pre.putStrLn str

instance (MonadLog m) => MonadLog (ReaderT e m) where
  printLog = lift . printLog
  flushLog = lift flushLog

instance (MonadLog m) => MonadLog (StateT s m) where
  printLog = lift . printLog
  flushLog = lift flushLog

data Verbosity
  = Error
  | Normal
  | Verbose
  | Debug
  | ExtraDebug

whenDebug :: (MonadReader Options m) => m () -> m ()
whenDebug action = do
  d <- view debug
  e <- view extraDebug
  when (e || d) action

whenExtraDebug :: (MonadReader Options m) => m () -> m ()
whenExtraDebug action = do
  e <- view extraDebug
  when e action

putStrLn_ :: (MonadReader Options m, MonadLog m) => Verbosity -> String -> m ()
putStrLn_ verb s = do
  q <- view quiet
  v <- view verbose
  d <- view debug
  e <- view extraDebug
  when
    ( case verb of
        ExtraDebug -> not q && e
        Debug -> not q && (e || d)
        Verbose -> not q && (e || d || v)
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
  flushLog
  errorCount += 1

logWarn ::
  ( MonadReader Options m,
    MonadLog m
  ) =>
  String ->
  m ()
logWarn msg = do
  putStrLn_ Normal $ "WARNING: " ++ msg
  flushLog

{-------------------------------------------------------------------------
 - Step 3: Database manipulation
 -}

newtype Prefix = Prefix {getPrefix :: FilePath}
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Prefix where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON Prefix

yymmdd :: LocalTime -> String
yymmdd = formatTime defaultTimeLocale "%y%m%d"

normalizeExt :: String -> String
normalizeExt ext = case strToLower ext of
  ".jpeg" -> ".jpg"
  ".tiff" -> ".tif"
  ext' -> ext'

-- Given a directory /foo/bar, this calculates what the "key" directory is for
-- calculations related to the counter.
keyDir :: Maybe FilePath -> FilePath -> FilePath
keyDir Nothing dir = dir
keyDir _destDir@(Just dir) _ = dir

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
  preuse (nameCounter . ix prefix) >>= \case
    Just idx -> go idx
    Nothing -> do
      nameCounter . at prefix ?= 1
      go 1
  where
    go i = do
      nameCounter . ix prefix += 1
      preuse (nameReservations . ix prefix) >>= \case
        Just s | has (ix i) s -> go (succ i)
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
  maybeWithCounter path $ \prefix counter ->
    nameCounter . at (keyDir destDir (takeDirectory path) </> prefix)
      %= Just . \case
        Just count -> max count (succ counter)
        Nothing -> succ counter

maybeReserveCounter ::
  (MonadReader Options m, MonadState RenamerState m) =>
  Maybe FilePath ->
  FilePath ->
  m ()
maybeReserveCounter destDir path =
  maybeWithCounter path $ \prefix counter ->
    nameReservations
      . at (keyDir destDir (takeDirectory path) </> prefix)
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
  getPermissions :: FilePath -> m Dir.Permissions
  setPermissions :: FilePath -> Dir.Permissions -> m ()

makeReadonly :: (MonadFSWrite m) => FilePath -> m ()
makeReadonly f = do
  p <- getPermissions f
  setPermissions f (p {Dir.writeable = False})

makeNonExecutable :: (MonadFSWrite m) => FilePath -> m ()
makeNonExecutable f = do
  p <- getPermissions f
  setPermissions f (p {Dir.executable = False})

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
  getPermissions = Dir.getPermissions
  setPermissions = Dir.setPermissions

instance (MonadFSWrite m) => MonadFSWrite (ReaderT e m) where
  removeFile = lift . removeFile
  removeDirectory = lift . removeDirectory
  renameFile = (lift .) . renameFile
  copyFileWithMetadata = (lift .) . copyFileWithMetadata
  getPermissions = lift . getPermissions
  setPermissions = (lift .) . setPermissions

instance (MonadFSWrite m) => MonadFSWrite (StateT s m) where
  removeFile = lift . removeFile
  removeDirectory = lift . removeDirectory
  renameFile = (lift .) . renameFile
  copyFileWithMetadata = (lift .) . copyFileWithMetadata
  getPermissions = lift . getPermissions
  setPermissions = (lift .) . setPermissions

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
  TimeZone ->
  String ->
  [FileDetails] ->
  m ()
renderDetails tz label ds = do
  forM_ ds $ \d ->
    putStrLn_ Debug $
      label
        ++ d ^. filepath
        ++ maybe "" ((" @ " ++) . show . utcToLocalTime tz) (d ^. captureTime)
  flushLog

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
    _fileroot = dropExtension _filepath

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
gatherDetails = (fmap sort .) . concatMapM $ \entry -> do
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
            flushLog
            details <-
              parallelInterleaved
                =<< walkFileEntries recurse getFileDetails entry
            when stateful $
              encodeFile detailsFile details
            pure details
      else do
        putStrLn_ Normal $ "Gathering details from " ++ show entry
        flushLog
        parallelInterleaved
          =<< walkFileEntries recurse getFileDetails entry
  stopGlobalPool
  pure details

processDetails ::
  (MonadReader Options m, MonadState RenamerState m) =>
  Maybe FilePath ->
  [FileDetails] ->
  m ()
processDetails = mapM_ . registerFileDetails
  where
    registerFileDetails destDir d
      | isJust destDir = maybeUpdateNameCounter destDir (d ^. filepath)
      | otherwise = pure ()

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

photoGroupCaptureTime :: PhotoGroup -> UTCTime
photoGroupCaptureTime (PhotoGroup xs _) =
  case xs ^.. traverse . _1 . captureTime . _Just of
    (x : _) -> x
    _ -> error "Photo group with no capture time!"

instance ToJSON PhotoGroup where
  toEncoding = genericToEncoding JSON.defaultOptions

instance FromJSON PhotoGroup

data Renaming a b = Renaming
  { _renamingFrom :: a,
    _renamingTo :: b,
    _renamingFor :: Reason
  }
  deriving (Eq, Ord, Show, Generic)

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
groupDetailsByTime :: [FileDetails] -> [NonEmpty FileDetails]
groupDetailsByTime = NE.groupBy $ \x y ->
  x ^. filedir == y ^. filedir
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

exists :: (a -> Bool) -> NonEmpty a -> Bool
exists f = isJust . find f
{-# INLINE exists #-}

gatherRoots :: [NonEmpty FileDetails] -> [NonEmpty FileDetails]
gatherRoots = foldr' go []
  where
    go xs@(x :| []) rest
      | isNothing (x ^. captureTime) = case break (exists (f x)) rest of
          (_, []) -> xs : rest
          (before, y : ys) -> before ++ NE.cons x y : ys
    go xs rest = xs : rest
    {-# INLINE go #-}

    f x y
      | isJust (y ^. captureTime) = x ^. fileroot == y ^. fileroot
      | otherwise = False
    {-# INLINE f #-}

-- This is the most complex function in the renamer, since it's job is to turn
-- a set of file details into an identified set of files and photo groups.
groupPhotos ::
  TimeZone ->
  Maybe FilePath ->
  [FileDetails] ->
  [Either FileDetails PhotoGroup]
groupPhotos tz destDir =
  sortOn
    ( \case
        Left x -> x ^. captureTime
        Right g -> Just (photoGroupCaptureTime g)
    )
    . Prelude.map go
    . gatherRoots
    . keepGroupsIf (hasUniqueExts . NE.toList)
    . groupDetailsByTime
  where
    key = keyDir destDir

    go (d :| []) = case d ^. captureTime of
      Just tm ->
        Right $
          PhotoGroup
            ((d, ForTime tm) :| [])
            (prefixFromTime (key (d ^. filedir)) tz tm)
      Nothing -> Left d
    go ds =
      Right
        ( PhotoGroup
            (NE.map (id &&& reason) ds)
            (prefixFromTime (key getDir) tz getTime)
        )
      where
        reason d =
          maybe
            (ForBase (d ^. fileroot))
            ForTime
            (d ^. captureTime)

        getTime = case times of
          (tm : _) -> tm
          [] -> error "computeRenamings: unexpected times"
          where
            times = catMaybes (NE.toList (NE.map (^. captureTime) ds))

        (getDir :| _) = NE.map (^. filedir) ds

computeRenamings ::
  (MonadReader Options m, MonadState RenamerState m) =>
  Maybe FilePath ->
  [Either FileDetails PhotoGroup] ->
  m [Mapping]
computeRenamings destDir = concatMapM go
  where
    go (Left x) = [] <$ maybeReserveCounter destDir (x ^. filepath)
    go (Right (PhotoGroup xs (Prefix prefix))) = do
      num <- nextNameCounter (Prefix prefix)
      pure $
        NE.toList $
          NE.map
            ( \(x, r) ->
                Renaming
                  x
                  (goalPath destDir x (Prefix (takeBaseName prefix)) num)
                  r
            )
            xs

mappingLabel' ::
  TimeZone ->
  Renaming a FilePath ->
  FilePath ->
  FilePath ->
  String
mappingLabel' tz ren src dst =
  src
    ++ case ren ^. renamingFor of
      ForTime tm -> " (" ++ formattedTime tm ++ ")-> "
      ForBase name -> " [" ++ name ++ "]-> "
    ++ dst
  where
    formattedTime tm =
      formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" tm'
      where
        tm' = utcToLocalTime tz tm

mappingLabel :: TimeZone -> (a -> FilePath) -> Renaming a FilePath -> String
mappingLabel tz f ren =
  mappingLabel' tz ren (f (ren ^. renamingFrom)) (ren ^. renamingTo)

renderMappings ::
  (MonadReader Options m, MonadLog m) =>
  String ->
  TimeZone ->
  (a -> FilePath) ->
  [Renaming a FilePath] ->
  m ()
renderMappings label tz f rs = do
  forM_ rs $
    putStrLn_ Debug . (label ++) . mappingLabel tz f
  flushLog

-- | Entries that would rename a file to itself.
idempotentRenaming :: Mapping -> Bool
idempotentRenaming ren = ren ^. renamingFrom . filepath == ren ^. renamingTo

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
    rs' = nonOverlapped (^. renamingFrom . filepath) rs
    rs'' = nonOverlapped (^. renamingTo) (Prelude.map fst rs')

    nonOverlapped ::
      (Mapping -> FilePath) ->
      [Mapping] ->
      [(Mapping, [Mapping])]
    nonOverlapped f =
      foldr
        ( \rens rest ->
            let k r = case r ^. renamingFor of
                  ForBase _ ->
                    r ^. renamingFrom . filepath
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
        (groupRenamingsBy (^. renamingFrom . filepath) rs)
    )
    $ do
      let rs' =
            removeRedundantRenamings (^. renamingTo) $
              removeRedundantRenamings (^. renamingFrom . filepath) $
                filter (not . idempotentRenaming) $
                  rs
          (rs'', overlaps) = removeOverlappedRenamings rs'
      forM_ overlaps $ \(x, ys) -> do
        putStrLn_ Normal $ "Preferring this renaming:"
        putStrLn_ Normal $ "    " ++ mappingLabel tz (^. filepath) x
        putStrLn_ Normal $ "  over these:"
        forM_ ys $ \y ->
          putStrLn_ Normal $ "    " ++ mappingLabel tz (^. filepath) y
      flushLog
      pure rs''

{-------------------------------------------------------------------------
 - Step 6: Plan
 -}

type Mapping = Renaming FileDetails FilePath

type SimpleMapping = Renaming FilePath FilePath

nextUniqueNum :: (MonadState RenamerState m) => m Integer
nextUniqueNum = do
  uniqueIdx <- use uniqueCounter
  uniqueCounter += 1
  pure uniqueIdx

buildPlan ::
  ( MonadReader Options m,
    MonadState RenamerState m,
    MonadProc m
  ) =>
  [Mapping] ->
  m [SimpleMapping]
buildPlan plan = do
  pid <- getCurrentPid
  (_, xs, ys) <- foldrM (work pid) (mempty :: HashSet FilePath, [], []) plan
  pure $ xs ++ ys
  where
    work pid (Renaming src dst ren) (srcs, rest, post)
      | has (ix (strToLower dst)) srcs
          && strToLower srcPath /= strToLower dst = do
          uniqueIdx <- nextUniqueNum
          let tmp =
                takeDirectory dst
                  </> "tmp_"
                  ++ show pid
                  ++ "_"
                  ++ show uniqueIdx
          pure
            ( srcs & at (strToLower srcPath) ?~ (),
              Renaming srcPath tmp ren : rest,
              Renaming tmp dst ren : post
            )
      | otherwise =
          pure
            ( srcs & at (strToLower srcPath) ?~ (),
              Renaming srcPath dst ren : rest,
              post
            )
      where
        srcPath = src ^. filepath

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
    _scenarioMappings :: [SimpleMapping]
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
  TimeZone ->
  [FilePath] ->
  [FilePath] ->
  Maybe FilePath ->
  m ([FileDetails], [FileDetails])
scenarioDetails tz repos inputs destDir = do
  putStrLn_ Normal "Gathering details..."
  flushLog
  rds <-
    if null repos
      then pure []
      else do
        ds <- gatherDetails repos
        processDetails destDir ds
        pure ds
  whenDebug $ renderDetails tz "REPO-DETAIL: " rds
  ds <- do
    ds <- gatherDetails inputs
    processDetails destDir ds
    pure ds
  whenDebug $ renderDetails tz "FROM-DETAIL: " ds
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
      doRenameFiles ds = do
        putStrLn_ Normal $
          "Determining photo groups (from "
            ++ show (length ds)
            ++ " entries)..."
        flushLog

        let gs = groupPhotos tz _scenarioDestination ds
        whenExtraDebug $
          forM_ gs $
            putStrLn_ ExtraDebug . ("GROUP: " ++) . ppShow
        flushLog
        putStrLn_ Normal $
          "Computing renamings (from "
            ++ show (length gs)
            ++ " files and photo groups)..."
        flushLog
        srs <- computeRenamings _scenarioDestination gs
        whenExtraDebug $ renderMappings "SIMPLE: " tz (^. filepath) srs
        putStrLn_ Normal $
          "Cleaning up renamings (from "
            ++ show (length srs)
            ++ " initial renamings)..."
        flushLog
        rs <- cleanRenamings tz srs
        whenExtraDebug $ renderMappings "CLEAN: " tz (^. filepath) rs
        pure (gs, srs, rs)

      doBuildPlan rs = do
        putStrLn_ Normal $
          "Building renaming plan (from "
            ++ show (length rs)
            ++ " final renamings)..."
        flushLog
        p <- buildPlan rs
        whenDebug $ renderMappings "PLAN: " tz id p
        pure p

safeRemoveDirectory ::
  (MonadReader Options m, MonadLog m, MonadFSRead m, MonadFSWrite m) =>
  FilePath ->
  m ()
safeRemoveDirectory path = do
  entries <- listDirectory path
  when (null entries) $ do
    putStrLn_ Normal $ "- " ++ path
    flushLog
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
      flushLog
      renameFile src dst
      pure 0
  | otherwise = do
      putStrLn_ Verbose $ label src dst
      flushLog
      isFile <- doesFileExist dst
      if isFile
        then do
          logWarn $
            "Destination already exists, appending + suffix: "
              ++ label src dst
          safeMoveFile label src (dropExtension dst ++ "+" ++ takeExtension dst)
        else do
          copyFileWithMetadata src dst
          makeReadonly dst
          makeNonExecutable dst
          removeFile src
          pure 0

executePlan ::
  ( MonadReader Options m,
    MonadLog m,
    MonadFSRead m,
    MonadFSWrite m
  ) =>
  TimeZone ->
  [SimpleMapping] ->
  m Integer
executePlan tz plan = do
  putStrLn_ Normal $
    "Executing renaming plan ("
      ++ show (length plan)
      ++ " operations)..."
  flushLog
  results <- forM plan $ \ren@(Renaming src dst _) ->
    safeMoveFile (mappingLabel' tz ren) src dst
  let errors = sum results
  flushLog
  putStrLn_ Normal $ "Renaming completed with " ++ show errors ++ " errors"
  flushLog
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
