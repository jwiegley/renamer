{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Fixtures where

import Control.Lens
import Control.Monad (MonadPlus)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (sort, sortOn)
import Data.Maybe (fromJust)
import Data.Time
import Data.Time.Format.ISO8601
import Debug.Trace
import Renamer
import System.FilePath
import System.Process (Pid)
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

instance (Monad m) => MonadLog (StateT r m) where
  printLog = traceM

instance (Monad m) => MonadProc (StateT Env m) where
  getCurrentPid = use envPid

instance (Monad m) => MonadParallel (StateT r m) where
  parallelInterleaved = sequence
  stopGlobalPool = pure ()

instance (Monad m) => MonadChecksum (StateT Env m) where
  fileChecksum _ = pure ""

instance (Monad m) => MonadPhoto (StateT Env m) where
  photoCaptureDate path = do
    t <- use envFileTree
    pure $ case lookupPath t path of
      Just (FileEntry tm _) -> tm
      _ -> Nothing

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

instance (Monad m) => MonadFS (StateT Env m) where
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

instance (Monad m) => MonadJSON (StateT r m) where
  decodeFileStrict _ = pure Nothing
  encodeFile _ _ = pure ()

time :: String -> UTCTime
time = fromJust . iso8601ParseM

photo :: (Monad m) => FilePath -> String -> StateT Env m ()
photo path tm =
  envFileTree %= \t ->
    adjustPath
      t
      path
      (const (Just (FileEntry (Just (time tm)) 100)))

file :: (Monad m) => FilePath -> StateT Env m ()
file path =
  envFileTree %= \t ->
    adjustPath
      t
      path
      (const (Just (FileEntry Nothing 100)))

simpleRename :: FileDetails -> String -> String -> RenamedFile
simpleRename details name tm = RenamedFile details name (SimpleRename (time tm))

followBase :: FileDetails -> String -> String -> RenamedFile
followBase details name n = RenamedFile details name (FollowBase n)

followTime :: FileDetails -> String -> String -> RenamedFile
followTime details name n = RenamedFile details name (FollowTime n)

ls :: (MonadLog m) => StateT Env m ()
ls = do
  t <- use envFileTree
  printLog $ ppShow t

allPaths :: (Monad m) => StateT Env m [FilePath]
allPaths = go "" <$> use envFileTree
  where
    go name (FileEntry _ _) = [name]
    go name (DirEntry xs) =
      concatMap (\(n, x) -> go (name </> n) x) (sortOn fst (HM.toList xs))

runWithFixture :: (Monad m) => StateT Env m a -> m a
runWithFixture = flip evalStateT (Env 123 (DirEntry mempty))

renamer ::
  (MonadPlus m, MonadFail m) =>
  [FilePath] ->
  ([FileDetails] -> [RenamedFile] -> m ()) ->
  ([FileDetails] -> [RenamedFile] -> m ()) ->
  ([FileDetails] -> [RenamedFile] -> m ()) ->
  StateT Env m [FilePath]
renamer
  paths
  handleSimpleRenamings
  handleSiblings
  handleAllRenamings = do
    runAppT
      ( defaultOptions
          { _quiet = True,
            _recursive = True,
            _execute = True
          }
      )
      $ do
        details <- gatherDetails Nothing paths
        renamings <-
          renameFiles
            utc
            Nothing
            (\rs -> rs <$ lift (lift (lift (handleSimpleRenamings details rs))))
            (\rs -> rs <$ lift (lift (lift (handleSiblings details rs))))
            (\rs -> rs <$ lift (lift (lift (handleAllRenamings details rs))))
            pure
            details
        executePlan utc =<< buildPlan Nothing renamings
    allPaths

importer ::
  (MonadPlus m, MonadFail m) =>
  [FilePath] ->
  [FilePath] ->
  FilePath ->
  StateT Env m [FilePath]
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
      _ <- gatherDetails (Just destDir) paths
      gatherDetails (Just destDir) froms
        >>= renameFiles utc (Just destDir) pure pure pure pure
        >>= buildPlan (Just destDir)
        >>= executePlan utc
  allPaths
