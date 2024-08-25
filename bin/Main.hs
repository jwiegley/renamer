{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (forM_)
import Data.Maybe (isJust)
import Data.Time (TimeZone, getCurrentTime, getTimeZone)
import Fixtures (allPaths, loadDetails, runSimulationAtPid)
import GHC.Conc (setNumCapabilities)
import Options.Applicative hiding (command)
import Options.Applicative qualified as OA
import Renamer
  ( Command (..),
    HasOptions (..),
    HasScenario (..),
    MonadJSON (..),
    Options (Options),
    determineScenario,
    execute,
    renamerExecute,
    runAppT,
    safePruneDirectory,
    scenarioDetails,
  )
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (hFlush, stdout)

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
  tz <- getTimeZone =<< getCurrentTime
  errors <- case cmd of
    RenamePhotos repos ->
      renamePhotos opts tz repos [] Nothing
    ImportPhotos repos destDir inputs ->
      renamePhotos opts tz repos inputs (Just destDir)
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
          ( long "case-insensitive"
              <> help "Enable when renaming on a case insensitive filesystem"
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
                  <> help "Read previously calculated scenario from FILE"
              )
          )

renamePhotos ::
  Options ->
  TimeZone ->
  [FilePath] ->
  [FilePath] ->
  Maybe FilePath ->
  IO Integer
renamePhotos opts tz repos inputs destDir = do
  (s, errors) <- runAppT opts $ do
    s <-
      view scenarioFrom >>= \case
        Just fromPath -> do
          mres <- liftIO $ decodeFileStrict fromPath
          case mres of
            Just s -> pure s
            Nothing -> error $ "Failed to read scenario from " ++ fromPath
        Nothing -> do
          (rds, ds) <- scenarioDetails repos inputs destDir
          determineScenario tz rds ds destDir
    mtoPath <- view scenarioTo
    forM_ mtoPath $ \toPath ->
      liftIO $ encodeFile toPath s
    exe <- view execute
    if exe
      then (s,) <$> go s
      else pure (s, 0)

  if opts ^. execute
    then pure errors
    else do
      hFlush stdout
      let (errors', paths) =
            runSimulationAtPid (fromIntegral (s ^. scenarioPid)) $ do
              loadDetails (s ^. scenarioRepository)
              loadDetails (s ^. scenarioInputs)
              errs <- runAppT opts $ go s
              (errs,) <$> allPaths
      hFlush stdout
      putStrLn "Resulting pathnames would be:"
      mapM_ putStrLn paths
      pure (errors + errors')
  where
    go s = do
      errors <- renamerExecute tz s
      when (errors == 0 && isJust destDir) $
        mapM_ safePruneDirectory inputs
      pure errors
