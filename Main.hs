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

import Control.Concurrent.ParallelIO
import Control.Lens hiding ((<.>))
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Aeson hiding (Options, (.=))
import Data.Char (toLower)
import Data.Foldable (foldrM, forM_)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.IntMap.Strict (IntMap)
import Data.IntSet qualified as S
import Data.List (group, nub, partition, sort)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Time
-- import Debug.Trace

import Data.Traversable (forM)
import GHC.Conc (setNumCapabilities)
import GHC.Generics (Generic)
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
  = SimpleRename UTCTime FilePath
  | SimpleRenameAvoidOverlap UTCTime FilePath
  | FollowBase FilePath FilePath
  | FollowTime FilePath FilePath
  deriving (Eq, Show)

makePrisms ''Renaming

data RenamedFile = RenamedFile
  { _sourceDetails :: FileDetails,
    -- | This is the renamed file, without the directory.
    _renamedFile :: FilePath,
    _renaming :: Renaming
  }
  deriving (Eq, Show)

makeLenses ''RenamedFile

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
    _execute :: !Bool,
    _keepState :: !Bool,
    _command :: Command
  }
  deriving (Show, Eq)

makeLenses ''Options

logErr :: (MonadIO m) => String -> AppT m ()
logErr msg = do
  liftIO $ Pre.putStrLn $ "ERROR: " ++ msg
  errorCount += 1

data Verbosity
  = Normal
  | Verbose
  | Debug

putStrLn_ :: (MonadIO m) => Verbosity -> String -> AppT m ()
putStrLn_ verb s = do
  q <- view quiet
  v <- view verbose
  d <- view debug
  when
    ( not q && case verb of
        Debug -> d
        Verbose -> d || v
        Normal -> True
    )
    $ liftIO
    $ Pre.putStrLn s

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
      ["-SubSecDateTimeOriginal", path]
      ""
  case ec of
    ExitSuccess ->
      Just
        <$> parseTimeM
          False
          defaultTimeLocale
          "Date/Time Original              : %0Y:%0m:%0d %0H:%0M:%0S%Q%Ez\n"
          out
    ExitFailure _code -> pure Nothing

getFileDetails :: Bool -> FilePath -> IO FileDetails
getFileDetails computeChecksum path = do
  isFile <- liftIO $ doesFileExist path
  unless isFile $
    error $
      "File does not exist: " ++ path
  _checksum <-
    if computeChecksum
      then liftIO $ Just <$> b3sum path
      else pure Nothing
  let _filepath = path
      _filedir = takeDirectory path
      _filename = takeFileName path
      _filebase = takeBaseName path
      _fileext = takeExtension path
  _captureTime <-
    if isImage _fileext
      then
        liftIO $
          exiftoolImageTimestamp path
            <|> exiv2ImageTimestamp path
      else pure Nothing
  let _fileIdx = -1
  _fileModTime <- liftIO $ getModificationTime path
  _fileSize <- liftIO $ getFileSize path
  pure FileDetails {..}

registerFileDetails :: Bool -> FileDetails -> AppT IO FileDetails
registerFileDetails gather FileDetails {..} = do
  when gather $
    registerCounter _filepath
  _fileIdx <- registerPath (_filedir </> _filename) _checksum
  pure FileDetails {..}

walkFileEntries ::
  (MonadIO m) =>
  (FilePath -> m a) ->
  FilePath ->
  m [m a]
walkFileEntries f path = do
  isDir <- liftIO $ doesDirectoryExist path
  if isDir
    then do
      liftIO $ Pre.putStrLn $ "Gathering details from " ++ show path
      concatMapM (walkFileEntries f . (path </>))
        =<< liftIO (listDirectory path)
    else pure [f path]

-- | Takes a list of files and/or directories, and gathers file details for
--   all files involved, recursively.
gatherDetails :: Bool -> [FilePath] -> AppT IO [FileDetails]
gatherDetails gather = concatMapM $ \entry -> do
  -- Get info on all entries; this is stateful and builds up the following
  -- tables:
  --   filepathToIdx
  --   idxToFilepath
  --   fileChecksums
  c <- view checksums
  isDir <- liftIO $ doesDirectoryExist entry
  details <-
    if isDir
      then do
        let detailsFile = entry </> ".file-details.json"
        isFile <- liftIO $ doesFileExist detailsFile
        stateful <- view keepState
        mres <-
          if stateful && isFile
            then liftIO $ decodeFileStrict detailsFile
            else pure Nothing
        case mres of
          Just (details, st) -> do
            lift $ put st
            pure details
          Nothing -> do
            details <-
              liftIO $
                parallelInterleaved =<< walkFileEntries (getFileDetails c) entry
            st <- lift get
            liftIO $ encodeFile detailsFile (details, st)
            pure details
      else
        liftIO $
          parallelInterleaved =<< walkFileEntries (getFileDetails c) entry
  liftIO stopGlobalPool
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
siblingRenamings ::
  [FileDetails] ->
  [RenamedFile] ->
  [Either String RenamedFile]
siblingRenamings xs rs = evalState (concatMapM go rs) (renamedFrom, renamedTo)
  where
    siblings :: HashMap FilePath [FileDetails]
    siblings =
      Prelude.foldl' (\m x -> addToList (x ^. filebase) x m) mempty xs

    renamedFrom :: HashSet FilePath
    renamedFrom =
      Prelude.foldl'
        (\m (RenamedFile d _ _) -> m & at (d ^. filename) ?~ ())
        mempty
        rs

    renamedTo :: HashSet FilePath
    renamedTo =
      Prelude.foldl' (\m (RenamedFile _ nm _) -> m & at nm ?~ ()) mempty rs

    go (RenamedFile details newname _) =
      case siblings
        ^? ix (details ^. filebase)
          . to (partition (\y -> y ^. fileext == details ^. fileext)) of
        Just (ys, zs)
          | ys == [details] && hasUniqueExts zs ->
              mapM
                ( \z -> do
                    (renFrom, renTo) <- get
                    if (z ^. filename) `HS.member` renFrom
                      then
                        pure $
                          Left $
                            "WARNING: Already renamed from source: "
                              ++ z ^. filename
                              ++ " -> "
                              ++ name z
                      else
                        if name z `HS.member` renTo
                          then
                            pure $
                              Left $
                                "WARNING: Already renamed to target: "
                                  ++ z ^. filename
                                  ++ " -> "
                                  ++ name z
                          else do
                            let ren =
                                  RenamedFile
                                    z
                                    (name z)
                                    ( FollowBase
                                        (details ^. filename)
                                        (name z)
                                    )
                            _1 . at (z ^. filename) ?= ()
                            _2 . at (name z) ?= ()
                            pure $ Right ren
                )
                zs
        _ -> pure []
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
simpleRenamings tz = do
  fmap reverse
    . foldrM go []
    . M.toDescList
    . Prelude.foldl'
      (\m x -> maybe m (\tm -> addToList tm x m) (x ^. captureTime))
      mempty
  where
    go (tm, entries) rest = do
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
            work ren base details = (RenamedFile details named (ren named) :)
              where
                named = name details
                name d = base <.> ext d
                ext d = normalizeExt (d ^. fileext)

            expectedBase = newName (yymmdd tm') <$> nextSeqNum (yymmdd tm')
              where
                tm' = utcToLocalTime tz tm
                newName base seqNum = base ++ "_" ++ printf "%04d" seqNum

-- | Remove entries that would rename a file to itself.
removeNeedlessRenamings :: [RenamedFile] -> [RenamedFile]
removeNeedlessRenamings =
  filter (\ren -> ren ^. sourceDetails . filename /= ren ^. renamedFile)

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
renameFiles ::
  (MonadIO m) =>
  TimeZone ->
  [FileDetails] ->
  AppT m [RenamedFile]
renameFiles tz ds = do
  rs <- removeNeedlessRenamings <$> simpleRenamings tz ds
  removeNeedlessRenamings <$> go rs
  where
    go rs = do
      rss' <- forM (siblingRenamings ds rs) $ \case
        Right ren -> pure [ren]
        Left msg -> [] <$ putStrLn_ Normal msg
      pure $ rs ++ concat rss'

{-------------------------------------------------------------------------
 - Step 6: Plan
 -}

hasUniqueExts :: [FileDetails] -> Bool
hasUniqueExts =
  null . duplicatedElements . map (normalizeExt . (^. fileext))

reportOverlaps ::
  (MonadIO m, MonadFail m) =>
  String ->
  ((Int, Int, a) -> Int) ->
  [(Int, Int, a)] ->
  [Int] ->
  AppT m ()
reportOverlaps label f plan = mapM_ $ \x ->
  forM_ (filter (\p -> f p == x) plan) $ \(src, dst, _) ->
    preuse (idxToFilepath . ix src) >>= \case
      Nothing ->
        logErr $ "Failed to find path entry for source " ++ show src
      Just (srcPath, _) ->
        preuse (idxToFilepath . ix dst) >>= \case
          Nothing ->
            logErr $ "Failed to find path entry for dest " ++ show dst
          Just (dstPath, _) ->
            logErr $
              "Overlapped "
                ++ label
                ++ ": "
                ++ srcPath
                ++ " -> "
                ++ dstPath

buildPlan ::
  (MonadIO m, MonadFail m) =>
  Maybe FilePath ->
  [RenamedFile] ->
  AppT m [(Int, Int, RenamedFile)]
buildPlan destDir rs = do
  pid <- liftIO getCurrentPid
  rs' <- foldrM go [] rs
  safeguardPlan pid rs'
  where
    go ren rest
      | ren ^. sourceDetails . filename == ren ^. renamedFile =
          error $ "Renaming file to itself: " ++ show ren
      | otherwise = do
          idx <-
            registerPath
              ( fromMaybe (ren ^. sourceDetails . filedir) destDir
                  </> ren ^. renamedFile
              )
              (ren ^. sourceDetails . checksum)
          pure $ (ren ^. sourceDetails . fileIdx, idx, ren) : rest

    safeguardPlan pid plan = do
      reportOverlaps "src" (\(x, _, _) -> x) plan (duplicatedElements srcs)
      reportOverlaps "dst" (\(_, x, _) -> x) plan (duplicatedElements dsts)
      uncurry (++) <$> foldrM work ([], []) plan
      where
        (srcs, dsts, _) = unzip3 plan
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

safeRemoveDirectory :: (MonadIO m) => FilePath -> AppT m ()
safeRemoveDirectory path = do
  putStrLn_ Normal $ "- " ++ path
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
  putStrLn_ Normal $ "- " ++ path
  dry <- not <$> view execute
  unless dry $ liftIO $ removeFile path

safeMoveFile :: String -> FilePath -> Maybe Checksum -> FilePath -> AppT IO ()
safeMoveFile label src srcSum dst
  | strToLower src == strToLower dst = do
      putStrLn_ Verbose $ src ++ " => " ++ dst
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
          putStrLn_ Verbose $ src ++ label ++ dst
          dry <- not <$> view execute
          unless dry $ do
            csum <- case srcSum of
              Just csum -> pure csum
              Nothing -> do
                isFile <- liftIO $ doesFileExist src
                -- This can be False if we are not in --execute mode, and the
                -- move is from a temp file that was never created.
                if isFile
                  then liftIO $ b3sum src
                  else pure ""
            isFile <- liftIO $ doesFileExist dst
            if isFile
              then do
                csum' <- liftIO $ b3sum dst
                if csum == csum'
                  then safeRemoveFile src
                  else logErr $ "Destination already exists: " ++ dst
              else do
                liftIO $ copyFileWithMetadata src dst
                csum' <- liftIO $ b3sum dst
                if csum == csum'
                  then safeRemoveFile src
                  else logErr $ "Checksum mismatch: " ++ csum ++ " != " ++ csum'
        else do
          putStrLn_ Normal $ "INFO: Duplicate: " ++ src
          safeRemoveFile src

executePlan :: TimeZone -> [(Int, Int, RenamedFile)] -> AppT IO ()
executePlan tz plan = do
  errors <- use errorCount
  if errors > 0
    then logErr "Cannot execute renaming plan with errors"
    else do
      putStrLn_ Normal $
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
      SimpleRename tm _ -> " (" ++ formattedTime tm ++ ")-> "
      SimpleRenameAvoidOverlap tm _ -> " (" ++ formattedTime tm ++ ")!> "
      FollowBase name _ -> " [" ++ name ++ "]-> "
      FollowTime name _ -> " {" ++ name ++ "}-> "

{-------------------------------------------------------------------------
 - Step 8: Commands
 -}

buildAndExecutePlan :: Bool -> [FilePath] -> Maybe FilePath -> AppT IO ()
buildAndExecutePlan gather dirs mdest =
  doGatherDetails >>= doRenameFiles >>= doBuildPlan >>= doExecutePlan
  where
    doGatherDetails = do
      putStrLn_ Normal "Gathering details..."
      details <- sort <$> gatherDetails gather dirs
      d <- view debug
      when d $
        forM_ details $ \det ->
          putStrLn_ Debug $
            det ^. filepath
              ++ maybe "" ((" @ " ++) . show) (det ^. captureTime)
      pure details

    doRenameFiles details = do
      putStrLn_ Normal $
        "Determining expected file names (from "
          ++ show (length details)
          ++ " entries)..."
      tz <- liftIO $ getTimeZone =<< getCurrentTime
      renamings <- renameFiles tz details
      d <- view debug
      when d $
        forM_ renamings $ \ren ->
          putStrLn_ Debug $
            ren ^. sourceDetails . filepath
              ++ " >> "
              ++ show (ren ^. renaming)
      pure renamings

    doBuildPlan renamings = do
      putStrLn_ Normal $
        "Building renaming plan (from "
          ++ show (length renamings)
          ++ " renamings)..."
      plan <- buildPlan mdest renamings
      v <- view verbose
      d <- view debug
      when d $
        forM_ plan $ \(src, dst, ren) -> do
          Just (srcPath, _) <- use (idxToFilepath . at src)
          Just (dstPath, _) <- use (idxToFilepath . at dst)
          putStrLn_ Debug $
            srcPath ++ " >>> " ++ dstPath
          when v $ liftIO $ pPrint ren
      pure plan

    doExecutePlan plan = do
      tz <- liftIO $ getTimeZone =<< getCurrentTime
      executePlan tz plan

renamePhotos :: [FilePath] -> AppT IO ()
renamePhotos = buildAndExecutePlan True ?? Nothing

importPhotos :: [FilePath] -> FilePath -> [FilePath] -> AppT IO ()
importPhotos froms toPath dirs = do
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
  _ <- GHC.Conc.setNumCapabilities (opts ^. jobs)
  errors <- runAppT opts $ do
    case opts ^. command of
      RenamePhotos dirs ->
        renamePhotos dirs
      ImportPhotos froms toPath dirs ->
        importPhotos froms toPath dirs
    use errorCount
  if errors == 0
    then exitSuccess
    else exitWith (ExitFailure errors)
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
              <> help "Do not report any progress"
          )
        <*> switch
          ( short 'v'
              <> long "verbose"
              <> help "Report progress verbosely"
          )
        <*> switch
          ( short 'd'
              <> long "debug"
              <> help "Report progress at debug level"
          )
        <*> option
          auto
          ( short 'j'
              <> long "jobs"
              <> value 1
              <> help "Number of concurrent IO jobs (default: 1)"
          )
        <*> switch
          ( long "checksum"
              <> help "Compute file checksums to detect duplicates"
          )
        <*> switch
          ( long "execute"
              <> help "Execute the renaming plan instead of just displaying it"
          )
        <*> switch
          ( long "keep-state"
              <> help "Keep state in .file-details.json (to aid debugging)"
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
