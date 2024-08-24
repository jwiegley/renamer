{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Fixtures where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (sort, sortOn)
import Data.Maybe (fromJust)
import Data.Time
import Data.Time.Format.ISO8601
import Debug.Trace
import GHC.Generics
import Renamer
import System.FilePath
import System.Process (Pid)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Show.Pretty

(@?==) :: (Eq a, Show a, HasCallStack, MonadIO m) => a -> a -> m ()
actual @?== expected = liftIO $ assertEqual' "" expected actual

assertEqual' :: (Eq a, Show a, HasCallStack) => String -> a -> a -> Assertion
assertEqual' preface expected actual =
  unless (actual == expected) $ assertFailure msg
  where
    msg =
      (if null preface then "" else preface ++ "\n")
        ++ "expected: "
        ++ ppShow expected
        ++ "\n but got: "
        ++ ppShow actual

data FileTree
  = FileEntry (Maybe UTCTime) Integer
  | DirEntry (HashMap FilePath FileTree)
  deriving (Eq, Show)

makePrisms ''FileTree

data Env = Env
  { _envPid :: Pid,
    _envFileTree :: FileTree
  }
  deriving (Show)

makeLenses ''Env

newtype Simulation a = Simulation {getSimulation :: State Env a}
  deriving
    ( Generic,
      Functor,
      Applicative,
      Monad,
      MonadState Env
    )

instance MonadLog Simulation where
  printLog = traceM

instance MonadProc Simulation where
  getCurrentPid = use envPid

instance MonadParallel Simulation where
  parallelInterleaved = sequence
  stopGlobalPool = pure ()

instance MonadPhoto Simulation where
  photoCaptureDate path = do
    t <- use envFileTree
    pure $ case lookupPath t path of
      Just (FileEntry tm _) -> tm
      _ -> Nothing

instance MonadJSON Simulation where
  decodeFileStrict _ = error "decodeFileStrict not used"
  encodeFile _ _ = error "encodeFile not used"

lookupPath :: FileTree -> FilePath -> Maybe FileTree
lookupPath tree path =
  foldl' go (Just tree) (splitDirectories path)
  where
    go (Just (DirEntry xs)) s
      | Just x <- xs ^? ix s = Just x
    go _ _ = Nothing

adjustPath ::
  FileTree ->
  FilePath ->
  (Maybe FileTree -> Maybe FileTree) ->
  FileTree
adjustPath tree path f = go tree (splitDirectories path)
  where
    go _ [] = error "Empty path"
    go (FileEntry _ _) _ = error "Attempt to descend into file"
    go (DirEntry xs) (s : ss) = DirEntry (xs & at s .~ work)
      where
        work = case xs ^? ix s of
          Nothing
            | null ss -> case f Nothing of
                Nothing -> error $ "Missing entry " ++ s
                Just x -> Just x
            | otherwise -> Just (go (DirEntry mempty) ss)
          Just x
            | null ss -> f (Just x)
            | otherwise -> Just (go x ss)

loadDetails :: [FileDetails] -> Simulation ()
loadDetails = mapM_ $ \d ->
  envFileTree %= \t ->
    adjustPath
      t
      (d ^. filepath)
      (const (Just (FileEntry (d ^. captureTime) (d ^. filesize))))

instance MonadFSRead Simulation where
  listDirectory path = do
    t <- use envFileTree
    case lookupPath t path of
      Just (DirEntry xs) -> pure $ sort $ HM.keys xs
      _ -> error $ "Not a directory: " ++ path
  doesFileExist path = do
    t <- use envFileTree
    pure $ case lookupPath t path of
      Just (FileEntry _ _) -> True
      _ -> False
  doesDirectoryExist path = do
    t <- use envFileTree
    pure $ case lookupPath t path of
      Just (DirEntry _) -> True
      _ -> False
  getFileSize path = do
    t <- use envFileTree
    case lookupPath t path of
      Just (FileEntry _ sz) -> pure sz
      _ -> error $ "Not a file: " ++ path

instance MonadFSWrite Simulation where
  removeFile path = do
    t <- use envFileTree
    case lookupPath t path of
      Just (FileEntry _ _) ->
        envFileTree .= adjustPath t path (const Nothing)
      _ -> error $ "Not a file: " ++ path
  removeDirectory path = do
    t <- use envFileTree
    case lookupPath t path of
      Just (DirEntry _) ->
        envFileTree .= adjustPath t path (const Nothing)
      _ -> error $ "Not a directory: " ++ path
  renameFile path dest = do
    t <- use envFileTree
    case lookupPath t path of
      Just x@(FileEntry _ _) ->
        envFileTree
          .= adjustPath
            (adjustPath t path (const Nothing))
            dest
            (const (Just x))
      _ -> error $ "Not a file: " ++ path
  copyFileWithMetadata path dest = do
    t <- use envFileTree
    case lookupPath t path of
      Just x@(FileEntry _ _) ->
        envFileTree .= adjustPath t dest (const (Just x))
      _ -> error $ "Not a file: " ++ path

time :: String -> UTCTime
time = fromJust . iso8601ParseM

photo :: FilePath -> String -> Simulation ()
photo path tm =
  envFileTree %= \t ->
    adjustPath
      t
      path
      (const (Just (FileEntry (Just (time tm)) 100)))

file :: FilePath -> Simulation ()
file path =
  envFileTree %= \t ->
    adjustPath
      t
      path
      (const (Just (FileEntry Nothing 100)))

simpleRename :: FileDetails -> String -> String -> Renamed FilePath
simpleRename details name tm = Renamed details name (SimpleRename (time tm))

followBase :: FileDetails -> String -> String -> Renamed FilePath
followBase details name n = Renamed details name (FollowBase n)

followTime :: FileDetails -> String -> String -> Renamed FilePath
followTime details name n = Renamed details name (FollowTime n)

ls :: Simulation ()
ls = do
  t <- use envFileTree
  printLog $ ppShow t

allPaths :: Simulation [FilePath]
allPaths = go "" <$> use envFileTree
  where
    go name (FileEntry _ _) = [name]
    go name (DirEntry xs) =
      concatMap (\(n, x) -> go (name </> n) x) (sortOn fst (HM.toList xs))

runSimulation :: Simulation a -> a
runSimulation = flip (evalState . getSimulation) (Env 123 (DirEntry mempty))

runSimulationAtPid :: Pid -> Simulation a -> a
runSimulationAtPid pid =
  flip (evalState . getSimulation) (Env pid (DirEntry mempty))

confirmScenario ::
  (MonadLog m, MonadIO m, MonadProc m) =>
  Scenario ->
  m ()
confirmScenario Scenario {..} = do
  _scenarioRenamings @?== renamings
  _scenarioMappings @?== mappings
  where
    (renamings, mappings) =
      runSimulationAtPid (fromIntegral _scenarioPid) $
        runAppT
          ( defaultOptions
              { _quiet = True,
                -- _quiet = False,
                -- _verbose = True,
                -- _debug = True,
                _recursive = True,
                _execute = True
              }
          )
          runScenario

    runScenario :: AppT Simulation (RenamingSet, [Mapping])
    runScenario = do
      (rds, ds) <- doProcessDetails
      rs <-
        doRenameFiles
          ( if null ds
              then rds
              else ds
          )
      ms <- doBuildPlan (rs ^. allRenamings)
      pure (rs, ms)

    doProcessDetails = do
      putStrLn_ Normal "Processing details..."
      rds <-
        if null _scenarioRepository
          then pure []
          else processDetails _scenarioDestination (sort _scenarioRepository)
      whenDebug $ renderDetails rds
      ds <- processDetails _scenarioDestination (sort _scenarioInputs)
      whenDebug $ renderDetails ds
      pure (rds, ds)

    doRenameFiles details = do
      putStrLn_ Normal $
        "Determining expected file names (from "
          ++ show (length details)
          ++ " entries)..."
      rs <-
        renameFiles
          (minutesToTimeZone _scenarioTimeZoneMinutes)
          _scenarioDestination
          details
      whenDebug $ renderRenamings (rs ^. allRenamings)
      pure rs

    doBuildPlan rs = do
      putStrLn_ Normal $
        "Building renaming plan (from "
          ++ show (length rs)
          ++ " renamings)..."
      p <- buildPlan _scenarioDestination rs
      whenDebug $ renderMappings p
      pure p

testScenario :: FilePath -> TestTree
testScenario path =
  testCase path $ do
    decodeFileStrict path >>= \case
      Just scenario -> confirmScenario scenario
      Nothing -> error $ "Failed to read scenario from " ++ path

renamerNoIdemCheck ::
  [FilePath] ->
  [FilePath] ->
  Maybe FilePath ->
  Simulation ((Scenario, Integer), [FilePath])
renamerNoIdemCheck
  _scenarioRepositories
  _scenarioInputs
  _scenarioDestination = do
    res <- runAppT
      ( defaultOptions
          { _quiet = True,
            -- _quiet = False,
            -- _verbose = True,
            -- _debug = True,
            _recursive = True,
            _execute = True
          }
      )
      $ do
        s <-
          determineScenario
            utc
            _scenarioRepositories
            _scenarioInputs
            _scenarioDestination
        (s,) <$> renamerExecute utc s
    (res,) <$> allPaths

renamer ::
  (MonadIO m) =>
  [FilePath] ->
  [FilePath] ->
  Maybe FilePath ->
  Simulation () ->
  m (Scenario, [FilePath])
renamer repos inputs destDir setup = do
  errors @?== 0
  errors' @?== 0
  paths @?== paths'
  pure (scenario, paths')
  where
    (scenario, errors, paths, errors', paths') = runSimulation $ do
      setup
      ((s, es), ps) <- renamerNoIdemCheck repos inputs destDir
      ((_, es'), ps') <- renamerNoIdemCheck (reverse ps) [] Nothing
      pure (s, es, ps, es', ps')

importer ::
  (MonadIO m) =>
  [FilePath] ->
  [FilePath] ->
  FilePath ->
  Simulation () ->
  m [FilePath]
importer paths froms destDir setup = do
  errors' @?== 0
  pure paths'
  where
    (errors', paths') = runSimulation $ do
      setup
      es <- runAppT
        ( defaultOptions
            { _quiet = True,
              -- _quiet = False,
              -- _verbose = True,
              -- _debug = True,
              _recursive = True,
              _execute = True
            }
        )
        $ do
          _ <-
            gatherDetails paths
              >>= processDetails (Just destDir)
          gatherDetails froms
            >>= processDetails (Just destDir)
            >>= renameFiles utc (Just destDir)
            >>= buildPlan (Just destDir) . (^. allRenamings)
            >>= executePlan utc
      (es,) <$> allPaths

type AppT m = ReaderT Options (StateT RenamerState m)

runAppT :: (Monad m) => Options -> AppT m a -> m a
runAppT opts k = evalStateT (runReaderT k opts) newRenamerState
