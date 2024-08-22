{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Fixtures where

import Control.Applicative (Alternative)
import Control.Lens
import Control.Monad (MonadPlus, unless)
import Control.Monad.IO.Class
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
import Test.Tasty.HUnit
import Text.Show.Pretty

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

newtype EnvT m a = EnvT {runEnvT :: StateT Env m a}
  deriving
    ( Generic,
      Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState Env,
      Alternative,
      MonadPlus,
      MonadTrans,
      MonadFail
    )

instance (Monad m) => MonadLog (EnvT m) where
  printLog = traceM

instance (Monad m) => MonadProc (EnvT m) where
  getCurrentPid = use envPid

instance (Monad m) => MonadParallel (EnvT m) where
  parallelInterleaved = sequence
  stopGlobalPool = pure ()

instance (Monad m) => MonadChecksum (EnvT m) where
  fileChecksum _ = pure ""

instance (Monad m) => MonadPhoto (EnvT m) where
  photoCaptureDate path = do
    t <- use envFileTree
    pure $ case lookupPath t path of
      Just (FileEntry tm _) -> tm
      _ -> Nothing

instance (Monad m) => MonadJSON (EnvT m) where
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

instance (Monad m) => MonadFS (EnvT m) where
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

photo :: (Monad m) => FilePath -> String -> EnvT m ()
photo path tm =
  envFileTree %= \t ->
    adjustPath
      t
      path
      (const (Just (FileEntry (Just (time tm)) 100)))

file :: (Monad m) => FilePath -> EnvT m ()
file path =
  envFileTree %= \t ->
    adjustPath
      t
      path
      (const (Just (FileEntry Nothing 100)))

simpleRename :: FileDetails -> String -> String -> RenamedFile
simpleRename details name tm = RenamedFile details name (SimpleRename (time tm))

simpleRenameAvoidOverlap :: FileDetails -> String -> String -> RenamedFile
simpleRenameAvoidOverlap details name tm =
  RenamedFile details name (SimpleRenameAvoidOverlap (time tm))

followBase :: FileDetails -> String -> String -> RenamedFile
followBase details name n = RenamedFile details name (FollowBase n)

followTime :: FileDetails -> String -> String -> RenamedFile
followTime details name n = RenamedFile details name (FollowTime n)

ls :: (MonadLog m) => EnvT m ()
ls = do
  t <- use envFileTree
  printLog $ ppShow t

allPaths :: (Monad m) => EnvT m [FilePath]
allPaths = go "" <$> use envFileTree
  where
    go name (FileEntry _ _) = [name]
    go name (DirEntry xs) =
      concatMap (\(n, x) -> go (name </> n) x) (sortOn fst (HM.toList xs))

runWithFixture :: (Monad m) => EnvT m a -> m a
runWithFixture = flip (evalStateT . runEnvT) (Env 123 (DirEntry mempty))

renamerNoIdemCheck ::
  (MonadLog m, MonadIO m, MonadPlus m, MonadFail m) =>
  [FilePath] ->
  ([FileDetails] -> [RenamedFile] -> m ()) ->
  ([FileDetails] -> [RenamedFile] -> m ()) ->
  ([FileDetails] -> [RenamedFile] -> m ()) ->
  EnvT m [FilePath]
renamerNoIdemCheck
  paths
  handleSimpleRenamings
  handleSiblings
  handleAllRenamings = do
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
      $ do
        details <- processDetails Nothing =<< gatherDetails paths

        putStrLn_ Verbose $
          "Determining expected file names (from "
            ++ show (length details)
            ++ " entries)..."
        renamings <-
          renameFiles
            utc
            Nothing
            (\rs -> rs <$ lift (lift (lift (handleSimpleRenamings details rs))))
            (\rs -> rs <$ lift (lift (lift (handleSiblings details rs))))
            (\rs -> rs <$ lift (lift (lift (handleAllRenamings details rs))))
            pure
            pure
            details
        whenDebug $ renderRenamings renamings

        putStrLn_ Verbose $
          "Building renaming plan (from "
            ++ show (length renamings)
            ++ " renamings)..."
        plan <- buildPlan Nothing renamings
        whenDebug $ renderMappings plan

        executePlan utc plan

        errors <- use errorCount
        lift $ errors @?== 0
    allPaths

renamer ::
  (MonadIO m, MonadPlus m, MonadFail m, MonadLog m) =>
  [FilePath] ->
  ([FileDetails] -> [RenamedFile] -> m ()) ->
  ([FileDetails] -> [RenamedFile] -> m ()) ->
  ([FileDetails] -> [RenamedFile] -> m ()) ->
  EnvT m [FilePath]
renamer paths handleSimpleRenamings handleSiblings handleAllRenamings = do
  paths' <-
    renamerNoIdemCheck
      paths
      handleSimpleRenamings
      handleSiblings
      handleAllRenamings
  paths'' <-
    renamerNoIdemCheck
      (reverse paths')
      (\_ _ -> pure ())
      (\_ _ -> pure ())
      (\_ _ -> pure ())
  liftIO $ paths'' @?= paths'
  pure paths'

importer ::
  (MonadPlus m, MonadFail m) =>
  [FilePath] ->
  [FilePath] ->
  FilePath ->
  EnvT m [FilePath]
importer paths froms destDir = do
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
    $ do
      _ <-
        gatherDetails paths
          >>= processDetails (Just destDir)
      gatherDetails froms
        >>= processDetails (Just destDir)
        >>= renameFiles utc (Just destDir) pure pure pure pure pure
        >>= buildPlan (Just destDir)
        >>= executePlan utc
  allPaths

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
