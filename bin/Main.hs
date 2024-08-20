{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Lens hiding ((<.>))
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Foldable (forM_)
import Data.List (sort)
import Data.Time
import GHC.Conc (setNumCapabilities)
import Options.Applicative hiding (command)
import Options.Applicative qualified as OA
import Renamer
import System.Exit
import System.FilePath
import Text.Regex.TDFA.String ()
import Text.Show.Pretty
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
      renamings <- renameFiles tz mdest details
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
