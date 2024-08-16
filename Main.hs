{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Arrow (first)
import Control.Lens hiding ((<.>))
import Control.Monad (unless, when, (<=<))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Aeson hiding (Options)
import Data.Char (toLower)
import Data.Foldable (foldrM, forM_)
import Data.HashMap.Strict hiding (foldr')
import Data.IntMap.Strict hiding (foldr')
import Data.IntSet qualified as S
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Time
import GHC.Generics
import Options.Applicative hiding (command)
import Options.Applicative qualified as OA
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Text.Printf
import Text.Regex.TDFA
import Text.Regex.TDFA.String ()
import Text.Show.Pretty

{-------------------------------------------------------------------------
 - Step 1: Schema
 -}

strToLower :: String -> String
strToLower = Prelude.map toLower

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
    _fileModTime :: UTCTime,
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

getTime :: FileDetails -> UTCTime
getTime d = fromMaybe (d ^. fileModTime) (d ^. captureTime)

type DetailsMap = HashMap FilePath FileDetails

data Renaming
  = SimpleRename FilePath
  | FollowBase [(FilePath, Renaming)]
  | FollowTime [(FilePath, Renaming)]
  deriving (Eq, Show)

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
    _entriesAtBase :: Map FilePath [FileDetails],
    _renamedAtBase :: Map FilePath [(FileDetails, Renaming)],
    _renameForBase :: Map FilePath FilePath,
    -- | Mapping from YYmmddHHMMSS to a list of entries for that moment within
    --   a given directory.
    _entriesAtTime :: Map (FilePath, UTCTime) [FileDetails],
    _renamedAtTime :: Map (FilePath, UTCTime) [(FileDetails, Renaming)],
    _renameForTime :: Map (FilePath, UTCTime) FilePath,
    -- | Mapping from renamed target to source locations
    _renamings :: Map FilePath [(FileDetails, Renaming)],
    -- | Mapping from checksums to a list of entries sharing that checksum
    _fileChecksums :: HashMap Checksum [FilePath],
    -- | A unique counter used to name temporary files
    _uniqueCounter :: Int
  }
  deriving (Show)

makeLenses ''RenamerState

newRenamerState :: RenamerState
newRenamerState =
  RenamerState
    { _filepathToIdx = mempty,
      _idxToFilepath = mempty,
      _fileIdxCounter = 0,
      _dailyCounter = mempty,
      _entriesAtBase = mempty,
      _renamedAtBase = mempty,
      _renameForBase = mempty,
      _entriesAtTime = mempty,
      _renamedAtTime = mempty,
      _renameForTime = mempty,
      _renamings = mempty,
      _fileChecksums = mempty,
      _uniqueCounter = 0
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

registerEntry :: (Monad m) => FileDetails -> AppT m ()
registerEntry details = do
  entriesAtBase %= addToList (details ^. filebase) details
  forM_ (details ^. captureTime) $ \stamp ->
    entriesAtTime %= addToList (details ^. filedir, stamp) details

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
  q <- view quiet
  unless q $ liftIO $ putStrLn s

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
  when gather $
    registerCounter _filepath
  _captureTime <-
    if isImage _fileext
      then
        liftIO $
          exiv2ImageTimestamp path
            <|> exiftoolImageTimestamp path
      else pure Nothing
  _fileIdx <- registerPath (_filedir </> _filename) _checksum
  _fileModTime <- liftIO $ getModificationTime path
  _fileSize <- liftIO $ getFileSize path
  let details = FileDetails {..}
  registerEntry details
  pure details

concatMapM :: (Traversable t, Monad m) => (a -> m [b]) -> t a -> m [b]
concatMapM = (fmap concat .) . mapM

-- | Takes a list of files and/or directories, and gathers file details for
--   all files involved, recursively.
gatherDetails :: Bool -> [FilePath] -> AppT IO [FileDetails]
gatherDetails gather =
  -- Get info on all entries; this is stateful and builds up the following
  -- tables:
  --   filepathToIdx
  --   idxToFilepath
  --   fileChecksums
  concatMapM $ \entry -> do
    isDir <- liftIO $ doesDirectoryExist entry
    if isDir
      then do
        let detailsFile = entry </> ".file-details.json"
        isFile <- liftIO $ doesFileExist detailsFile
        mres <-
          if isFile
            then liftIO $ decodeFileStrict detailsFile
            else pure Nothing
        case mres of
          Just details -> pure details
          Nothing -> do
            details <- walkFileEntries (getFileDetails gather) entry
            liftIO $ encodeFile detailsFile details
            pure details
      else walkFileEntries (getFileDetails gather) entry

walkFileEntries ::
  (MonadIO m) =>
  (FilePath -> AppT m a) ->
  FilePath ->
  AppT m [a]
walkFileEntries f path = do
  isDir <- liftIO $ doesDirectoryExist path
  if isDir
    then do
      putStrLn_ $ "Gathering details from " ++ show path
      concatMapM (walkFileEntries f . (path </>))
        =<< liftIO (listDirectory path)
    else (: []) <$> f path

{-------------------------------------------------------------------------
 - Step 5: Naming
 -}

nextSeqNum :: (Monad m) => String -> AppT m Int
nextSeqNum yymmdd =
  zoom dailyCounter $
    preuse (ix yymmdd) >>= \case
      Just idx -> idx <$ (ix yymmdd += 1)
      Nothing -> 1 <$ (at yymmdd ?= 2)

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

-- | Determine the ideal name for a given photo, in the context of the
--   repository where it is meant to abide. Note that this function is called
--   only after all file details have been gathered throughout the various
--   photo repositories.
--
--   1. It should contain the date when the photo was taken.
--
--   2. It should contain the sequence of which photo it was that day.
--
--   3. jww (2024-08-13): If there are two files with the same basename but
--      different extensions, they should be renamed together.
--
--   4. jww (2024-08-13): If it is the alternate version (different extension)
--      of an existing photo, it should share the sequence number.
fileRename ::
  (MonadIO m, MonadFail m) =>
  FileDetails ->
  AppT m (Maybe (FilePath, Renaming))
fileRename details = case details ^. captureTime of
  Just tm -> do
    (newBase, renaming) <- determineNewBase tm
    renameForBase
      %= setIfMissing
        (details ^. filedir </> details ^. filebase)
        newBase
    renamedAtBase
      %= addToList
        (details ^. filedir </> details ^. filebase)
        (details, renaming)
    renameForTime %= setIfMissing (details ^. filedir, tm) newBase
    renamedAtTime %= addToList (details ^. filedir, tm) (details, renaming)
    let name = newBase <.> newExt
    if name == details ^. filename
      then pure Nothing
      else do
        preuse (renamings . ix name) >>= \case
          Just xs -> do
            liftIO $
              putStrLn $
                "Invalid rename: "
                  ++ details ^. filepath
                  ++ " -> "
                  ++ name
            liftIO $ pPrint renaming
            liftIO $ putStrLn $ "... already a renaming target for:"
            liftIO $ pPrint $ Prelude.map (first (^. filepath)) xs
            renamings . ix name %= ((details, renaming) :)
          Nothing -> renamings . at name ?= [(details, renaming)]
        renamings %= addToList name (details, renaming)
        pure $ Just (name, renaming)
  Nothing -> pure Nothing
  where
    determineNewBase tm = do
      -- Is there already a renaming for this basename, all with different
      -- extensions? If so, share that renaming.
      followBase <-
        preuse
          ( renameForBase
              . ix (details ^. filedir </> details ^. filebase)
          )
          >>= \case
            Just renaming -> do
              Just entries <-
                preuse
                  ( renamedAtBase
                      . ix (details ^. filedir </> details ^. filebase)
                  )
              pure $
                if newExt
                  `notElem` Prelude.map
                    (normalizeExt . (^. fileext) . fst)
                    entries
                  && all ((Just tm ==) . (^. captureTime) . fst) entries
                  then
                    Just
                      ( renaming,
                        FollowBase (Prelude.map (first (^. filepath)) entries)
                      )
                  else Nothing
            Nothing -> pure Nothing
      case followBase of
        Just x -> pure x
        Nothing -> do
          -- Is there already a renaming for this capture time, all with
          -- different extensions? If so, share that renaming.
          followTime <-
            preuse (renameForTime . ix (details ^. filedir, tm)) >>= \case
              Just renaming -> do
                Just entries <-
                  preuse (renamedAtTime . ix (details ^. filedir, tm))
                pure $
                  if newExt
                    `notElem` Prelude.map
                      (normalizeExt . (^. fileext) . fst)
                      entries
                    then
                      Just
                        ( renaming,
                          FollowTime (Prelude.map (first (^. filepath)) entries)
                        )
                    else Nothing
              Nothing -> pure Nothing
          case followTime of
            Just x -> pure x
            Nothing -> do
              let ymd = yymmdd tm
              seqNum <- nextSeqNum ymd
              let name = newName ymd seqNum
              pure (name, SimpleRename name)

    yymmdd = formatTime defaultTimeLocale "%y%m%d"

    newName base seqNum = base ++ "_" ++ printf "%04d" seqNum

    newExt = normalizeExt (details ^. fileext)

{-------------------------------------------------------------------------
 - Step 6: Plan
 -}

buildPlan ::
  (Monad m) =>
  Maybe FilePath ->
  [(FileDetails, (FilePath, Renaming))] ->
  AppT m [(Int, Int, Renaming)]
buildPlan destDir = safeguardPlan <=< foldrM go []
  where
    go (details, (fileName, renaming)) rest
      | details ^. filename == fileName = pure rest
      | otherwise = do
          idx <-
            registerPath
              ( fromMaybe (details ^. filedir) destDir
                  </> fileName
              )
              (details ^. checksum)
          pure $ (details ^. fileIdx, idx, renaming) : rest

    safeguardPlan plan = do
      -- unless (length srcs == S.size srcs') $
      --   liftIO $
      --     putStrLn "buildPlan: moving from same source multiple times"
      -- unless (length dsts == S.size dsts') $
      --   liftIO $
      --     putStrLn "buildPlan: moving to same destination multiple times"
      foldrM work [] plan
      where
        (srcs, _dsts, _) = unzip3 plan

        srcs' = S.fromList srcs
        _dsts' = S.fromList _dsts

        work (src, dst, ren) rest
          | S.member dst srcs' = do
              uniqueIdx <- nextUniqueNum
              (dstPath, csum) <- use (idxToFilepath . ix dst)
              idx <-
                registerPath
                  (takeDirectory dstPath </> "tmp_" ++ show uniqueIdx)
                  csum
              pure $ (src, idx, ren) : rest ++ [(idx, dst, ren)]
          | otherwise =
              pure $ (src, dst, ren) : rest

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

safeMoveFile :: String -> FilePath -> Maybe Checksum -> FilePath -> AppT IO ()
safeMoveFile label src srcSum dst
  | strToLower src == strToLower dst = do
      putStrLn_ $ src ++ " => " ++ dst
      dry <- not <$> view execute
      unless dry $ liftIO $ renameFile src dst
  | otherwise = do
      shouldMove <- case srcSum of
        Just csum ->
          preuse (fileChecksums . ix csum) >>= \case
            Just _ -> pure False
            _ -> pure True
        Nothing -> pure True
      if shouldMove
        then do
          putStrLn_ $ src ++ label ++ dst
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
          dry <- not <$> view execute
          if isFile
            then do
              csum' <- liftIO $ b3sum dst
              if csum == csum'
                then safeRemoveFile src
                else
                  unless dry $
                    liftIO $
                      putStrLn $
                        "MOVE FAILED, destination already exists: " ++ dst
            else unless dry do
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

executePlan :: [(Int, Int, Renaming)] -> AppT IO ()
executePlan plan = forM_ plan $ \(src, dst, ren) -> do
  (srcPath, csum) <- use (idxToFilepath . ix src)
  (dstPath, _) <- use (idxToFilepath . ix dst)
  safeMoveFile (label ren) srcPath csum dstPath
  where
    label (SimpleRename _) = " -> "
    label (FollowBase _) = " -B> "
    label (FollowTime _) = " -T> "

{-------------------------------------------------------------------------
 - Step 8: Commands
 -}

buildAndExecutePlan :: Bool -> [FilePath] -> Maybe FilePath -> AppT IO ()
buildAndExecutePlan gather dirs mdest = do
  details <- sort <$> gatherDetails gather dirs
  mapM fileRename details
    >>= buildPlan mdest . winnowRenames . zip details
    >>= executePlan
  where
    winnowRenames = Prelude.foldr go []
      where
        go (_, Nothing) rest = rest
        go (x, Just y) rest = (x, y) : rest

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
          ( short 'q'
              <> long "quiet"
              <> help "Do not report progress verbosely"
          )
        <*> switch
          ( long "checksum"
              <> help "Compute file checksums to detect duplicates"
          )
        <*> switch
          ( long "execute"
              <> help "Execute the renaming plan instead of just displaying it"
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
