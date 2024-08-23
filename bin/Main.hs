{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encodeFile)
import Data.Foldable (forM_)
import Data.Maybe (isJust)
import Data.Time (TimeZone, getCurrentTime, getTimeZone)
import Fixtures (AppT, allPaths, loadDetails, runAppT, runSimulationAtPid)
import GHC.Conc (setNumCapabilities)
import Options.Applicative hiding (command)
import Options.Applicative qualified as OA
import Renamer
  ( Command (..),
    Options (..),
    Verbosity (Normal),
    determineScenario,
    execute,
    putStrLn_,
    renamerExecute,
    safePruneDirectory,
    scenarioInputs,
    scenarioPid,
    scenarioRepository,
    scenarioTo,
  )
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

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
  _ <- GHC.Conc.setNumCapabilities (_jobs opts)
  tz <- getTimeZone =<< getCurrentTime
  errors <- runAppT opts $ case cmd of
    RenamePhotos repos ->
      renamePhotos tz repos [] Nothing
    ImportPhotos repos destDir inputs ->
      renamePhotos tz repos inputs (Just destDir)
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

renamePhotos ::
  TimeZone ->
  [FilePath] ->
  [FilePath] ->
  Maybe FilePath ->
  AppT IO Integer
renamePhotos tz repos inputs mdest = do
  scenario <- determineScenario tz repos inputs mdest
  mtoPath <- view scenarioTo
  forM_ mtoPath $ \toPath ->
    liftIO $ encodeFile toPath scenario

  opts <- view id
  exe <- view execute
  if exe
    then go scenario
    else do
      let (errors, paths) =
            runSimulationAtPid (fromIntegral (scenario ^. scenarioPid)) $ do
              loadDetails (scenario ^. scenarioRepository)
              loadDetails (scenario ^. scenarioInputs)
              errs <- runAppT opts $ go scenario
              (errs,) <$> allPaths
      forM_ paths $ putStrLn_ Normal
      pure errors
  where
    go scenario = do
      errors <- renamerExecute tz scenario
      when (errors == 0 && isJust mdest) $
        mapM_ safePruneDirectory inputs
      pure errors
