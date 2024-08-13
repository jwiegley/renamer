{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens hiding ((<.>))
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Char (toLower)
import Data.Foldable (foldrM, forM_)
import Data.HashMap.Strict hiding (foldr')
import Data.IntMap.Strict hiding (foldr')
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Traversable (forM)
import Options.Applicative as OA
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Text.Printf
import Text.Regex.TDFA
import Text.Regex.TDFA.String ()

{-------------------------------------------------------------------------
 - Step 1: Schema
 -}

type Checksum = String

data FileType
  = RawFile
  | TiffFile
  | PsdFile
  | JpegFile
  | PngFile
  | XmpFile
  | UnknownExtension
  deriving (Show, Eq, Ord)

makePrisms ''FileType

isImage :: FileType -> Bool
isImage = \case
  RawFile -> True
  TiffFile -> True
  PsdFile -> True
  JpegFile -> True
  PngFile -> True
  _ -> False

data FileDetails = FileDetails
  { _captureTime :: Maybe UTCTime,
    _fileModTime :: UTCTime,
    _filepath :: FilePath, -- "/foo/bar.CR3"
    _filedir :: FilePath, -- "/foo"
    _filename :: FilePath, -- "bar.CR3"
    _filebase :: FilePath, -- "bar"
    _fileIdx :: Int,
    _fileext :: FilePath, -- ".CR3"
    _checksum :: Maybe Checksum, -- "<hex string>"
    _fileType :: FileType,
    _fileSize :: Integer
  }
  deriving (Eq, Ord, Show)

makeLenses ''FileDetails

-- | State of the image repository (new or old).
data RenamerState = RenamerState
  { _filepathToIdx :: HashMap FilePath Int,
    _idxToFilepath :: IntMap (FilePath, Maybe Checksum),
    -- | Mapping from YYmmdd to a sequence counter for that day.
    _dailyCounter :: HashMap String Int,
    _entriesAtTime :: HashMap String [FileDetails],
    _fileIdxCounter :: Int,
    _uniqueCounter :: Int,
    _fileChecksums :: HashMap Checksum [FilePath]
  }
  deriving (Show)

makeLenses ''RenamerState

newRenamerState :: RenamerState
newRenamerState =
  RenamerState
    { _filepathToIdx = mempty,
      _idxToFilepath = mempty,
      _dailyCounter = mempty,
      _entriesAtTime = mempty,
      _fileIdxCounter = 0,
      _uniqueCounter = 0,
      _fileChecksums = mempty
    }

{-------------------------------------------------------------------------
 - Step 2: Environment
 -}

data Options = Options
  { _verbose :: !Bool,
    _checksums :: !Bool,
    _dryRun :: !Bool,
    _transfers :: Maybe ([FilePath], FilePath),
    _repositories :: [FilePath]
  }
  deriving (Show, Eq)

makeLenses ''Options

type AppT m = ReaderT Options (StateT RenamerState m)

runAppT :: (Monad m) => Options -> AppT m a -> m a
runAppT opts k = evalStateT (runReaderT k opts) newRenamerState

{-------------------------------------------------------------------------
 - Step 3: Database manipulation
 -}

-- | Register a path name, return its unique integer identifier.
registerPath :: (Monad m) => FilePath -> Maybe Checksum -> AppT m Int
registerPath path mcsum = do
  forM_ mcsum $ \csum ->
    preuse (fileChecksums . ix path) >>= \case
      Just paths
        | path `elem` paths -> pure ()
        | otherwise -> fileChecksums . ix csum %= (path :)
      Nothing -> fileChecksums . at csum ?= [path]

  preuse (filepathToIdx . ix path) >>= \case
    Just idx -> pure idx
    Nothing -> do
      idx <- use fileIdxCounter
      fileIdxCounter += 1
      filepathToIdx . at path ?= idx
      idxToFilepath . at idx ?= (path, mcsum)
      pure idx

registerCounter :: (Monad m) => FilePath -> AppT m ()
registerCounter path =
  case path
         =~ ( "^([0-9][0-9][0-9][0-9][0-9][0-9])_([0-9][0-9][0-9][0-9])$" ::
                String
            ) ::
         [[String]] of
    [(_ : yymmdd : counter : [])] ->
      forM_
        ( parseTimeM False defaultTimeLocale "%0y%0m%0d" yymmdd ::
            Maybe UTCTime
        )
        $ \_ ->
          dailyCounter . at yymmdd ?= read counter
    _ -> pure ()

getTime :: FileDetails -> UTCTime
getTime d = fromMaybe (d ^. fileModTime) (d ^. captureTime)

{-------------------------------------------------------------------------
 - Step 4: Analyze
 -}

b3sum :: FilePath -> IO String
b3sum path = do
  (ec, out, err) <-
    readProcessWithExitCode
      "b3sum"
      ["--no-names", "--quiet", path]
      ""
  case ec of
    ExitSuccess -> pure $ init out
    ExitFailure code -> do
      putStrLn $ "b3sum failed, code " ++ show code
      putStrLn $ "error: " ++ err
      pure ""

exiv2ImageTimestamp :: FilePath -> IO (Maybe UTCTime)
exiv2ImageTimestamp path = do
  (ec, out, _err) <-
    readProcessWithExitCode
      "exiv2"
      ["-g", "Exif.Image.DateTime", "-Pv", path]
      ""
  case ec of
    ExitSuccess ->
      Just
        <$> parseTimeM
          False
          defaultTimeLocale
          "%0Y:%0m:%0d %0H:%0M:%0S\n"
          out
    ExitFailure _code -> do
      -- putStrLn $ "exiv2 failed, code " ++ show code
      -- putStrLn $ "error: " ++ err
      pure Nothing

exiftoolImageTimestamp :: FilePath -> IO (Maybe UTCTime)
exiftoolImageTimestamp path = do
  (ec, out, _err) <-
    readProcessWithExitCode
      "exiftool"
      ["-DateTimeOriginal", path]
      ""
  case ec of
    ExitSuccess ->
      Just
        <$> parseTimeM
          False
          defaultTimeLocale
          "Date/Time Original              : %0Y:%0m:%0d %0H:%0M:%0S\n"
          out
    ExitFailure _code -> do
      -- putStrLn $ "exiftool failed, code " ++ show code
      -- putStrLn $ "error: " ++ err
      pure Nothing

getFileDetails :: Bool -> FilePath -> AppT IO FileDetails
getFileDetails gather path = do
  isFile <- liftIO $ doesFileExist path
  unless isFile $
    error $
      "File does not exist: " ++ path
  computeChecksum <- view checksums
  _checksum <-
    if computeChecksum
      then liftIO $ Just <$> b3sum path
      else pure Nothing
  let _filepath = path
      _filedir = takeDirectory path
      _filename = takeFileName path
      _filebase = takeBaseName path
      _fileext = takeExtension path
      _fileType = case Prelude.map toLower _fileext of
        ".crw" -> RawFile
        ".cr2" -> RawFile
        ".cr3" -> RawFile
        ".tif" -> TiffFile
        ".tiff" -> TiffFile
        ".psd" -> PsdFile
        ".jpg" -> JpegFile
        ".jpeg" -> JpegFile
        ".png" -> PngFile
        ".xmp" -> XmpFile
        _ -> UnknownExtension
  _fileIdx <-
    registerPath
      (_filedir </> Prelude.map toLower _filename)
      _checksum
  -- jww (2024-08-12): Instead of using --reset, use sub-commands to reflect
  -- when renaming of an existing repository should be done.
  when gather $
    registerCounter _filepath
  _captureTime <-
    if isImage _fileType
      then
        liftIO $
          exiv2ImageTimestamp path
            <|> exiftoolImageTimestamp path
      else pure Nothing
  _fileModTime <- liftIO $ getModificationTime path
  _fileSize <- liftIO $ getFileSize path
  pure FileDetails {..}

-- | Takes a list of files and/or directories, and gathers file details for
--   all files involved, recursively.
gatherDetails :: [(FilePath, Bool)] -> AppT IO [FileDetails]
gatherDetails entries =
  -- Get info on all entries; this is stateful and builds up the following
  -- tables:
  --   filepathToIdx
  --   idxToFilepath
  --   fileChecksums
  fmap concat $ forM entries $ \(entry, gather) -> do
    liftIO $ putStrLn $ "Gathering details from " ++ show entry
    walkFileEntries (getFileDetails gather) entry

walkFileEntries :: (MonadIO m) => (FilePath -> m a) -> FilePath -> m [a]
walkFileEntries f path = do
  isDir <- liftIO $ doesDirectoryExist path
  if isDir
    then do
      paths <- liftIO $ listDirectory path
      fmap concat $ mapM (walkFileEntries f . (path </>)) paths
    else (: []) <$> f path

{-------------------------------------------------------------------------
 - Step 5: Naming
 -}

-- | Determine the ideal name for a given photo, in the context of the
--   repository where it is meant to abide.
--
--   1. It should contain the date when the photo was taken.
--
--   2. It should contain the sequence of which photo it was that day.
--
--   3. jww (2024-08-13): If it is the alternate version (different extension)
--      of an existing photo, it should share the sequence number:
--
--      - 240813_0001.CR3
--      - 240813_0001.JPG
--
--      The alternate version is detected both either checking for the same
--      basename (A.CR3, A.JPG) or for the same data+time of capture, although
--      only the latter of these two is needed.
idealName :: (Monad m) => FileDetails -> AppT m FilePath
idealName = zoom dailyCounter . generateBaseName . getTime
  where
    generateBaseName tm =
      baseName yymmdd
        <$> ( preuse (ix yymmdd)
                >>= \case
                  Just idx -> idx <$ (ix yymmdd += 1)
                  Nothing -> 1 <$ (at yymmdd ?= 2)
            )
      where
        yymmdd = formatTime defaultTimeLocale "%y%m%d" tm

        baseName :: String -> Int -> String
        baseName s n = s ++ "_" ++ printf "%04d" n

{-------------------------------------------------------------------------
 - Step 6: Plan
 -}

buildPlan ::
  (Monad m) =>
  [(FileDetails, FilePath)] ->
  AppT m [(Int, Int)]
buildPlan xs = do
  plan <- foldrM go [] xs
  let srcs = sort (Prelude.map fst plan)
      dsts = sort (Prelude.map snd plan)
  unless (srcs == nub srcs) $
    error "buildPlan failure: moving from same source multiple times"
  unless (dsts == nub dsts) $
    error "buildPlan failure: moving to same destination multiple times"
  pure plan
  where
    go (details, baseName) rest
      | details ^. filebase == baseName = pure rest
      | otherwise = do
          idx <-
            registerPath
              ( details ^. filedir
                  </> Prelude.map
                    toLower
                    (baseName <.> details ^. fileext)
              )
              Nothing
          pure $ (details ^. fileIdx, idx) : rest

safeguardPlan :: (Monad m) => [(Int, Int)] -> AppT m [(Int, Int)]
safeguardPlan = foldrM go []
  where
    go (src, dst) rest
      | dst `elem` Prelude.map fst rest = do
          uniqueIdx <- use uniqueCounter
          uniqueCounter += 1
          (dstPath, _) <- use (idxToFilepath . ix dst)
          idx <-
            registerPath
              (takeDirectory dstPath </> "tmp_" ++ show uniqueIdx)
              Nothing
          pure $ (src, idx) : rest ++ [(idx, dst)]
      | otherwise = pure $ (src, dst) : rest

{-------------------------------------------------------------------------
 - Step 7: Execute
 -}

executePlan :: Bool -> [(Int, Int)] -> AppT IO ()
executePlan dry plan = do
  -- opts <- use options
  forM_ plan $ \(src, dst) -> do
    (srcPath, csum) <- use (idxToFilepath . ix src)
    (dstPath, _) <- use (idxToFilepath . ix dst)
    liftIO $ safeMoveFile dry srcPath csum dstPath

safePruneDirectory :: Bool -> FilePath -> IO ()
safePruneDirectory dry path = do
  entries <- listDirectory path
  safeToRemove <- flip execStateT True $
    forM_ entries $ \entry -> do
      let entryPath = path </> entry
      isDir <- liftIO $ doesDirectoryExist entryPath
      if isDir
        then liftIO $ safePruneDirectory dry entryPath
        else put False
  when safeToRemove $ do
    putStrLn $ "- " ++ path
    unless dry $
      removeDirectory path

safeMoveFile :: Bool -> FilePath -> Maybe Checksum -> FilePath -> IO ()
safeMoveFile dry src srcSum dest = do
  putStrLn $ src ++ " -> " ++ dest
  unless dry $ do
    csum <- case srcSum of
      Just csum -> pure csum
      Nothing -> b3sum src
    isFile <- doesFileExist dest
    if isFile
      then do
        csum' <- b3sum dest
        if csum == csum'
          then removeFile src
          else
            putStrLn $
              "MOVE FAILED, destination already exists: " ++ dest
      else do
        copyFileWithMetadata src dest
        csum' <- b3sum dest
        if csum == csum'
          then removeFile src
          else
            putStrLn $
              "MOVE FAILED, checksum mismatch: "
                ++ src
                ++ " -> "
                ++ dest
                ++ ": "
                ++ csum
                ++ " != "
                ++ csum'

{-------------------------------------------------------------------------
 - Step 8: Driver
 -}

version :: String
version = "0.0.1"

copyright :: String
copyright = "2024"

summary :: String
summary =
  "renamer "
    ++ version
    ++ ", (C) "
    ++ copyright
    ++ " John Wiegley"

-- jww (2024-08-13): Use subcommands to distinguish between importing,
-- renaming, name checking, validation, etc.
renamerOptions :: Parser Options
renamerOptions =
  Options
    <$> switch
      ( short 'v'
          <> long "verbose"
          <> help "Report progress verbosely"
      )
    <*> switch
      ( long "checksum"
          <> help "Compute file checksums to detect duplicates"
      )
    <*> switch
      ( long "dry-run"
          <> help "Show the execution plan instead of running it"
      )
    <*> optional
      ( (,)
          <$> some
            ( OA.strOption
                ( long "from"
                    <> help "Entries to move into --to directory"
                )
            )
          <*> OA.strOption
            ( long "to"
                <> help "Directory to move --from entries into"
            )
      )
    <*> some (OA.argument str (metavar "ENTRIES"))

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> renamerOptions)
    (fullDesc <> progDesc "" <> header summary)

getOptions :: IO Options
getOptions = execParser optionsDefinition

-- | All path entries implied by the option set.
allEntries :: Options -> [(FilePath, Bool)]
allEntries opts =
  Prelude.map (,True) (opts ^. repositories) ++ case opts ^. transfers of
    Nothing -> []
    Just (paths, path) -> (path, False) : Prelude.map (,True) paths

main :: IO ()
main = do
  opts <- getOptions
  runAppT opts $ do
    details <- sort <$> gatherDetails (allEntries opts)
    -- jww (2024-08-13): Only map idealName over the --from entries when
    -- importing.
    mapM idealName details
      >>= buildPlan . zip details
      >>= safeguardPlan
      >>= executePlan (opts ^. dryRun)
