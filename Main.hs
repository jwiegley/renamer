{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens hiding ((<.>))
import Control.Monad (unless, when, (<=<))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Char (toLower)
import Data.Foldable (foldrM, forM_)
import Data.HashMap.Strict hiding (foldr')
import Data.IntMap.Strict hiding (foldr')
import Data.IntSet qualified as S
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Traversable (forM)
import Options.Applicative hiding (command)
import Options.Applicative qualified as OA
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

type Checksum = String

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

getTime :: FileDetails -> UTCTime
getTime d = fromMaybe (d ^. fileModTime) (d ^. captureTime)

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

data Command
  = ImportPhotos [FilePath] FilePath [FilePath]
  | RenamePhotos [FilePath]
  deriving (Show, Eq)

makePrisms ''Command

data Options = Options
  { _verbose :: !Bool,
    _checksums :: !Bool,
    _execute :: !Bool,
    _command :: Command
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

{-------------------------------------------------------------------------
 - Step 4: Analyze
 -}

putStrLn_ :: (MonadIO m) => String -> AppT m ()
putStrLn_ s = do
  v <- view verbose
  when v $ liftIO $ putStrLn s

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
    ExitFailure _code -> pure Nothing

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
    ExitFailure _code -> pure Nothing

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
    registerPath (_filedir </> Prelude.map toLower _filename) _checksum
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
gatherDetails :: Bool -> [FilePath] -> AppT IO [FileDetails]
gatherDetails gather entries =
  -- Get info on all entries; this is stateful and builds up the following
  -- tables:
  --   filepathToIdx
  --   idxToFilepath
  --   fileChecksums
  fmap concat $ forM entries $ \entry -> do
    putStrLn_ $ "Gathering details from " ++ show entry
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
  Maybe FilePath ->
  [(FileDetails, FilePath)] ->
  AppT m [(Int, Int)]
buildPlan destDir = safeguardPlan <=< foldrM go []
  where
    go (details, baseName) rest
      | details ^. filebase == baseName = pure rest
      | otherwise = do
          idx <-
            registerPath
              ( fromMaybe (details ^. filedir) destDir
                  </> Prelude.map
                    toLower
                    (baseName <.> details ^. fileext)
              )
              (details ^. checksum)
          pure $ (details ^. fileIdx, idx) : rest

    safeguardPlan plan = do
      unless (length srcs == S.size srcs') $
        error "buildPlan: moving from same source multiple times"
      unless (length dsts == S.size dsts') $
        error "buildPlan: moving to same destination multiple times"
      foldrM work [] plan
      where
        (srcs, dsts) = unzip plan

        srcs' = S.fromList srcs
        dsts' = S.fromList dsts

        work (src, dst) rest
          | S.member dst srcs' = do
              uniqueIdx <- use uniqueCounter
              uniqueCounter += 1
              (dstPath, csum) <- use (idxToFilepath . ix dst)
              idx <-
                registerPath
                  (takeDirectory dstPath </> "tmp_" ++ show uniqueIdx)
                  csum
              pure $ (src, idx) : rest ++ [(idx, dst)]
          | otherwise =
              pure $ (src, dst) : rest

{-------------------------------------------------------------------------
 - Step 7: Execute
 -}

safeRemoveDirectory :: (MonadIO m) => FilePath -> AppT m ()
safeRemoveDirectory path = do
  putStrLn_ $ "- " ++ path
  dry <- not <$> view execute
  unless dry $ liftIO $ removeDirectory path

safePruneDirectory :: (MonadIO m) => FilePath -> AppT m ()
safePruneDirectory path = do
  entries <- liftIO $ listDirectory path
  safeToRemove <- flip execStateT True $
    forM_ entries $ \entry -> do
      let entryPath = path </> entry
      isDir <- liftIO $ doesDirectoryExist entryPath
      if isDir
        then lift $ safePruneDirectory entryPath
        else put False
  when safeToRemove $
    safeRemoveDirectory path

safeRemoveFile :: (MonadIO m) => FilePath -> AppT m ()
safeRemoveFile path = do
  putStrLn_ $ "- " ++ path
  dry <- not <$> view execute
  unless dry $ liftIO $ removeFile path

safeMoveFile :: FilePath -> Maybe Checksum -> FilePath -> AppT IO ()
safeMoveFile src srcSum dst = do
  shouldMove <- case srcSum of
    Just csum ->
      preuse (fileChecksums . ix csum) >>= \case
        Just _ -> pure False
        _ -> pure True
    Nothing -> pure True
  if shouldMove
    then do
      putStrLn_ $ src ++ " -> " ++ dst
      csum <- case srcSum of
        Just csum -> pure csum
        Nothing -> do
          isFile <- liftIO $ doesFileExist src
          -- This can be False if we are not in --execute mode, and the move
          -- is from a temp file that was never created.
          if isFile
            then liftIO $ b3sum src
            else pure ""
      isFile <- liftIO $ doesFileExist dst
      if isFile
        then do
          csum' <- liftIO $ b3sum dst
          if csum == csum'
            then safeRemoveFile src
            else
              liftIO $
                putStrLn $
                  "MOVE FAILED, destination already exists: " ++ dst
        else do
          dry <- not <$> view execute
          unless dry $ do
            liftIO $ copyFileWithMetadata src dst
            csum' <- liftIO $ b3sum dst
            if csum == csum'
              then safeRemoveFile src
              else
                liftIO $
                  putStrLn $
                    "MOVE FAILED, checksum mismatch: "
                      ++ csum
                      ++ " != "
                      ++ csum'
    else do
      putStrLn_ $ "Duplicate: " ++ src
      safeRemoveFile src

executePlan :: [(Int, Int)] -> AppT IO ()
executePlan plan = forM_ plan $ \(src, dst) -> do
  (srcPath, csum) <- use (idxToFilepath . ix src)
  (dstPath, _) <- use (idxToFilepath . ix dst)
  safeMoveFile srcPath csum dstPath

{-------------------------------------------------------------------------
 - Step 8: Commands
 -}

buildAndExecutePlan :: Bool -> [FilePath] -> Maybe FilePath -> AppT IO ()
buildAndExecutePlan gather dirs mdest = do
  details <- sort <$> gatherDetails gather dirs
  mapM idealName details
    >>= buildPlan mdest . zip details
    >>= executePlan

renamePhotos :: [FilePath] -> AppT IO ()
renamePhotos dirs = buildAndExecutePlan True dirs Nothing

importPhotos :: [FilePath] -> FilePath -> [FilePath] -> AppT IO ()
importPhotos froms toPath dirs = do
  -- Gather details to determine daily sequence numbers, checksums, etc.
  _ <- gatherDetails True dirs
  buildAndExecutePlan False froms (Just toPath)
  mapM_ safePruneDirectory froms

{-------------------------------------------------------------------------
 - Step 9: Driver
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

main :: IO ()
main = do
  opts <- getOptions
  runAppT opts $ case opts ^. command of
    RenamePhotos dirs -> renamePhotos dirs
    ImportPhotos froms toPath dirs -> importPhotos froms toPath dirs
  where
    getOptions :: IO Options
    getOptions = execParser optionsDefinition
      where
        optionsDefinition :: ParserInfo Options
        optionsDefinition =
          info
            (helper <*> renamerOptions)
            (fullDesc <> progDesc "" <> header summary)

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
        <*> hsubparser
          ( importCommand
              <> renameCommand
          )
      where
        importCommand :: Mod CommandFields Command
        importCommand =
          OA.command
            "import"
            (info importOptions (progDesc "Import photos"))
          where
            importOptions :: Parser Command
            importOptions =
              ImportPhotos
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
                <*> some (OA.argument str (metavar "ENTRIES"))

        renameCommand :: Mod CommandFields Command
        renameCommand =
          OA.command
            "rename"
            (info renameOptions (progDesc "Rename photos"))
          where
            renameOptions :: Parser Command
            renameOptions =
              RenamePhotos
                <$> some (OA.argument str (metavar "ENTRIES"))
