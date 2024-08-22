{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens hiding ((<.>))
import Control.Monad.IO.Class
import Data.List (sort)
import Data.Time
import GHC.Conc (setNumCapabilities)
import Options.Applicative hiding (command)
import Options.Applicative qualified as OA
import Renamer
import System.Exit
import System.FilePath
import Text.Regex.TDFA.String ()
import Prelude hiding (putStrLn)

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
  (opts, cmd) <- getOptions
  _ <- GHC.Conc.setNumCapabilities (opts ^. jobs)
  errors <- runAppT opts $ do
    case cmd of
      RenamePhotos dirs ->
        renamePhotos dirs
      ImportPhotos froms toPath dirs ->
        importPhotos froms toPath dirs
    use errorCount
  if errors == 0
    then exitSuccess
    else exitWith (ExitFailure errors)
  where
    getOptions :: IO (Options, Command)
    getOptions = execParser optionsDefinition
      where
        optionsDefinition :: ParserInfo (Options, Command)
        optionsDefinition =
          info
            ( helper
                <*> ( (,)
                        <$> renamerOptions
                        <*> hsubparser
                          ( importCommand
                              <> renameCommand
                          )
                    )
            )
            (fullDesc <> progDesc "" <> header summary)

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
          ( short 'c'
              <> long "checksum"
              <> help "Compute file checksums to detect duplicates"
          )
        <*> switch
          ( short 'r'
              <> long "recursive"
              <> help "Recurse into directories provided on the command-line"
          )
        <*> switch
          ( long "execute"
              <> help "Execute the renaming plan instead of just displaying it"
          )
        <*> switch
          ( long "keep-state"
              <> help "Keep state in .file-details.json (to aid debugging)"
          )
        <*> switch
          ( long "span"
              <> help "Number photos within YYmmdd periods across directories"
          )
        <*> optional
          ( strOption
              ( long "write-scenario"
                  <> help "Write calculated scenario to FILE"
              )
          )
        <*> optional
          ( strOption
              ( long "read-scenario"
                  <> help "Read calculated scenario from FILE"
              )
          )

buildAndExecutePlan :: [FilePath] -> Maybe FilePath -> AppT IO ()
buildAndExecutePlan dirs mdest = do
  tz <- liftIO $ getTimeZone =<< getCurrentTime
  scenario <- do
    _scenarioDetails <- doGatherDetails
    _scenarioRenamings <- doRenameFiles tz _scenarioDetails
    _scenarioMappings <- doBuildPlan (_scenarioRenamings ^. allRenamings)
    pure Scenario {..}
  executePlan tz (scenario ^. scenarioMappings)
  where
    doGatherDetails = do
      putStrLn_ Normal "Gathering details..."
      ds <- sort <$> (processDetails mdest =<< gatherDetails dirs)
      whenDebug $ renderDetails ds
      pure ds

    doRenameFiles tz details = do
      putStrLn_ Normal $
        "Determining expected file names (from "
          ++ show (length details)
          ++ " entries)..."
      rs <- renameFiles tz mdest details
      whenDebug $ renderRenamings (rs ^. allRenamings)
      pure rs

    doBuildPlan renamings = do
      putStrLn_ Normal $
        "Building renaming plan (from "
          ++ show (length renamings)
          ++ " renamings)..."
      p <- buildPlan mdest renamings
      whenDebug $ renderMappings p
      pure p

renamePhotos :: [FilePath] -> AppT IO ()
renamePhotos = buildAndExecutePlan ?? Nothing

importPhotos :: [FilePath] -> FilePath -> [FilePath] -> AppT IO ()
importPhotos froms toPath dirs = do
  _ <- processDetails (Just toPath) =<< gatherDetails dirs
  buildAndExecutePlan froms (Just toPath)
  mapM_ safePruneDirectory froms
