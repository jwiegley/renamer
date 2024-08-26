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
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (sort, sortOn)
import Data.Time
import Data.Time.Format.ISO8601
import Debug.Trace
import GHC.Generics
import Renamer
import System.FilePath
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
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
  flushLog = pure $! unsafePerformIO (hFlush stdout)

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
lookupPath tree = foldl' go (Just tree) . splitDirectories . strToLower
  where
    go (Just (DirEntry xs)) s
      | Just x <- xs ^? ix s = Just x
    go _ _ = Nothing
    {-# INLINE go #-}

adjustPath ::
  FileTree ->
  Maybe FileTree ->
  FilePath ->
  FileTree
adjustPath tree mt = go tree . splitDirectories . strToLower
  where
    go (DirEntry xs) (s : ss) = DirEntry (xs & at s .~ work)
      where
        work
          | null ss = mt
          | otherwise =
              Just $ go (xs ^. at s . non (DirEntry mempty)) ss
        {-# INLINE work #-}
    go _ _ = error "Unexpected"
    {-# INLINE go #-}

loadDetails :: [FileDetails] -> Simulation ()
loadDetails = mapM_ $ \d ->
  envFileTree %= \t ->
    adjustPath
      t
      (Just (FileEntry (d ^. captureTime) (d ^. filesize)))
      (d ^. filepath)

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
        envFileTree .= adjustPath t Nothing path
      _ -> error $ "Not a file: " ++ path
  removeDirectory path = do
    t <- use envFileTree
    case lookupPath t path of
      Just (DirEntry _) ->
        envFileTree .= adjustPath t Nothing path
      _ -> error $ "Not a directory: " ++ path
  renameFile path dest = do
    t <- use envFileTree
    case lookupPath t path of
      Just x@(FileEntry _ _) ->
        envFileTree
          .= adjustPath (adjustPath t Nothing path) (Just x) dest
      _ -> error $ "Not a file: " ++ path
  copyFileWithMetadata path dest = do
    t <- use envFileTree
    case lookupPath t path of
      Just x@(FileEntry _ _) ->
        envFileTree .= adjustPath t (Just x) dest
      _ -> error $ "Not a file: " ++ path

time :: String -> UTCTime
time str = case iso8601ParseM str of
  Just tm -> tm
  Nothing -> error $ "Not an ISO8601 time string: " ++ str

photo :: FilePath -> String -> Simulation ()
photo path tm =
  envFileTree %= \t ->
    adjustPath t (Just (FileEntry (Just (time tm)) 100)) path

file :: FilePath -> Simulation ()
file path =
  envFileTree %= \t ->
    adjustPath t (Just (FileEntry Nothing 100)) path

forBase :: FileDetails -> String -> FilePath -> Mapping
forBase details name n = Renaming (details ^. filepath) name (ForBase n)

forTime :: FileDetails -> String -> String -> Mapping
forTime details name n = Renaming (details ^. filepath) name (ForTime (time n))

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
confirmScenario s = s' @?== s
  where
    s' =
      runSimulationAtPid (fromIntegral (s ^. scenarioPid))
        $ runAppT
          ( defaultOptions
              & quiet .~ True
              -- & quiet .~ False,
              -- & verbose .~ True,
              -- & debug .~ True,
              & recursive .~ True
              & execute .~ True
          )
        $ do
          determineScenario
            utc
            (s ^. scenarioRepository)
            (s ^. scenarioInputs)
            (s ^. scenarioDestination)

testScenario :: FilePath -> TestTree
testScenario path =
  testCase path $ do
    decodeFileStrict path >>= \case
      Just s -> confirmScenario s
      Nothing -> error $ "Failed to read scenario from " ++ path

renamerNoIdemCheck ::
  [FilePath] ->
  [FilePath] ->
  Maybe FilePath ->
  Simulation ((Scenario, Integer), [FilePath])
renamerNoIdemCheck repos inputs destDir = do
  res <- runAppT
    ( defaultOptions
        & quiet .~ True
        -- & quiet .~ False
        -- & verbose .~ True
        -- & debug .~ True
        & recursive .~ True
        & execute .~ True
    )
    $ do
      (rds, ds) <- scenarioDetails repos inputs destDir
      s <- determineScenario utc rds ds destDir
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
  pure (scen, paths')
  where
    (scen, errors, paths, errors', paths') = runSimulation $ do
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
            & quiet .~ True
            -- & quiet .~ False
            -- & verbose .~ True
            -- & debug .~ True
            & recursive .~ True
            & execute .~ True
        )
        $ do
          _ <-
            gatherDetails paths
              >>= processDetails (Just destDir)
          spanDirs <- view spanDirectories
          gatherDetails froms
            >>= processDetails (Just destDir)
            >>= computeRenamings (Just destDir)
              . groupPhotos spanDirs (Just destDir) utc
            >>= cleanRenamings utc
            >>= buildPlan
            >>= executePlan utc
      (es,) <$> allPaths
