{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Graph
import Data.HashMap.Strict
import Data.IntMap.Strict
import Data.Time
import Data.Time.Format
import Options.Applicative as OA
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Text.Show.Pretty

-- import Text.Regex.TDFA

type Checksum = String

-- | State of the image repository (new or old).
data RenamerState = RenamerState
  { _filepathToIdx :: HashMap FilePath Int,
    _idxToFilepath :: IntMap FilePath,
    -- | Mapping from YYYYmmdd to a sequence counter for that day.
    _dailyCounter :: HashMap String Int,
    _counter :: Int,
    _checksums :: HashMap Checksum [FilePath]
  }
  deriving (Show)

makeLenses ''RenamerState

newRenamerState :: RenamerState
newRenamerState =
  RenamerState
    { _filepathToIdx = mempty,
      _idxToFilepath = mempty,
      _dailyCounter = mempty,
      _counter = 0,
      _checksums = mempty
    }

data FileDetails = FileDetails
  { _filepath :: FilePath, -- "/foo/bar.CR3"
    _filedir :: FilePath, -- "/foo"
    _filename :: FilePath, -- "bar.CR3"
    _filebase :: FilePath, -- "bar"
    _filebaseIdx :: Int,
    _fileext :: FilePath, -- ".CR3"
    _checksum :: Checksum, -- "<hex string>"
    _captureTime :: Maybe UTCTime,
    _fileModTime :: UTCTime,
    _fileSize :: Integer
  }
  deriving (Eq, Show)

makeLenses ''FileDetails

data RepoState = RepoState
  { _fileDetails :: HashMap FilePath FileDetails
  }
  deriving (Show)

makeLenses ''RepoState

data Results = Results
  { mappings :: Graph
  }
  deriving (Show)

version :: String
version = "0.0.1"

copyright :: String
copyright = "2024"

summary :: String
summary =
  "org-data "
    ++ version
    ++ ", (C) "
    ++ copyright
    ++ " John Wiegley"

data Options = Options
  { _verbose :: !Bool,
    _files :: [FilePath]
  }
  deriving (Show, Eq)

makeLenses ''Options

options :: Parser Options
options =
  Options
    <$> switch
      ( short 'v'
          <> long "verbose"
          <> help "Report progress verbosely"
      )
    <*> some (OA.argument str (metavar "FILES"))

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> options)
    (fullDesc <> progDesc "" <> header summary)

getOptions :: IO Options
getOptions = execParser optionsDefinition

main :: IO ()
main = do
  opts <- getOptions
  pPrint opts
  let rst = newRenamerState
  details <-
    flip runStateT rst $
      mapM getFileDetails (_files opts)
  pPrint details

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
  (ec, out, err) <-
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
    ExitFailure code -> do
      -- putStrLn $ "exiv2 failed, code " ++ show code
      -- putStrLn $ "error: " ++ err
      pure Nothing

exiftoolImageTimestamp :: FilePath -> IO (Maybe UTCTime)
exiftoolImageTimestamp path = do
  (ec, out, err) <-
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
    ExitFailure code -> do
      -- putStrLn $ "exiftool failed, code " ++ show code
      -- putStrLn $ "error: " ++ err
      pure Nothing

registerPath :: FilePath -> Checksum -> StateT RenamerState IO Int
registerPath path csum = do
  preuse (checksums . ix path) >>= \case
    Just paths
      | path `elem` paths -> pure ()
      | otherwise -> checksums . ix csum %= (path :)
    Nothing -> checksums . at csum ?= [path]

  preuse (filepathToIdx . ix path) >>= \case
    Just idx -> pure idx
    Nothing -> do
      idx <- use counter
      counter += 1
      filepathToIdx . at path ?= idx
      idxToFilepath . at idx ?= path
      pure idx

getFileDetails :: FilePath -> StateT RenamerState IO FileDetails
getFileDetails path = do
  _checksum <- liftIO $ b3sum path
  idx <- registerPath path _checksum
  let _filepath = path
      _filedir = takeDirectory path
      _filename = takeFileName path
      _filebase = takeBaseName path
      _filebaseIdx = idx
      _fileext = takeExtension path
  _captureTime <-
    liftIO $
      exiv2ImageTimestamp path
        <|> exiftoolImageTimestamp path
  _fileModTime <- liftIO $ getModificationTime path
  _fileSize <- liftIO $ getFileSize path
  pure FileDetails {..}
