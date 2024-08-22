{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens hiding ((<.>))
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Foldable (forM_)
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
  errors <- runAppT opts $ case cmd of
    RenamePhotos repos ->
      renamePhotos repos
    ImportPhotos repos destDir inputs ->
      importPhotos repos inputs destDir
  if errors == 0
    then exitSuccess
    else exitWith (ExitFailure (fromIntegral errors))
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

buildAndExecutePlan ::
  [FilePath] ->
  [FilePath] ->
  Maybe FilePath ->
  AppT IO Integer
buildAndExecutePlan repos inputs mdest = do
  tz <- liftIO $ getTimeZone =<< getCurrentTime
  mfromPath <- view scenarioFrom
  scenario <- case mfromPath of
    Just fromPath ->
      decodeFileStrict fromPath >>= \case
        Just s
          | s ^. scenarioRepositories == repos
              && s ^. scenarioInputs == inputs
              && s ^. scenarioDestination == mdest ->
              pure s
        _ -> determineScenario tz repos inputs mdest
    Nothing -> determineScenario tz repos inputs mdest
  mtoPath <- view scenarioTo
  forM_ mtoPath $ encodeFile ?? scenario
  errors <- use (within . errorCount)
  if errors > 0
    then do
      logErr "Cannot execute renaming plan with errors"
      pure errors
    else executePlan tz (scenario ^. scenarioMappings)

renamePhotos :: [FilePath] -> AppT IO Integer
renamePhotos = buildAndExecutePlan [] ?? Nothing

importPhotos :: [FilePath] -> [FilePath] -> FilePath -> AppT IO Integer
importPhotos repos inputs destDir = do
  errors <- buildAndExecutePlan repos inputs (Just destDir)
  when (errors == 0) $ mapM_ safePruneDirectory inputs
  pure errors
