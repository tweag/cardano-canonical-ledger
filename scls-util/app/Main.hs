{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.SCLS.Util.Info
import Cardano.SCLS.Util.Result
import Cardano.SCLS.Util.Tool
import Cardano.SCLS.Util.Verify
import Data.Text (Text)
import Options.Applicative
import System.Exit (exitWith)

-- | Command-line options
data Options = Options
  { optCommand :: Command
  }

data Command
  = Verify FilePath
  | VerifyNamespace FilePath Text
  | Info FilePath
  | ListNamespaces FilePath
  | Split FilePath FilePath
  | Merge FilePath [FilePath]

parseOptions :: Parser Options
parseOptions =
  Options
    <$> subparser
      ( command
          "verify"
          ( info
              (Verify <$> fileArg)
              (progDesc "Verify root hash of chunks")
          )
          <> command
            "verify-ns"
            ( info
                (VerifyNamespace <$> fileArg <*> namespaceArg)
                (progDesc "Verify hash for a specific namespace")
            )
          <> command
            "info"
            ( info
                (Info <$> fileArg)
                (progDesc "Display SCLS file information")
            )
          <> command
            "list-ns"
            ( info
                (ListNamespaces <$> fileArg)
                (progDesc "List all namespaces in the file")
            )
          <> command
            "split"
            ( info
                (Split <$> fileArg <*> dirArg)
                (progDesc "Split SCLS file into separate files by namespace")
            )
          <> command
            "merge"
            ( info
                (Merge <$> fileArg <*> some (argument str (metavar "FILES")))
                (progDesc "Merge multiple SCLS files into one (last file is output)")
            )
      )
 where
  fileArg =
    argument
      str
      (metavar "FILE" <> help "Path to SCLS file")
  dirArg =
    argument
      str
      (metavar "OUTPUT_DIR" <> help "Output directory for split files")
  namespaceArg =
    argument
      str
      (metavar "NAMESPACE" <> help "Namespace identifier")

-- | Main entry point
main :: IO ()
main = do
  opts <- execParser $ info (parseOptions <**> helper) fullDesc
  result <- runCommand (optCommand opts)
  exitWith $ toErrorCode result

-- | Execute the selected command
runCommand :: Command -> IO Result
runCommand = \case
  Verify file -> verifyRoot file
  VerifyNamespace file ns -> verifyNamespace file ns
  Info file -> displayInfo file
  ListNamespaces file -> listNamespaces file
  Split file outputDir -> splitFile file outputDir
  Merge _ [] -> do
    putStrLn "Error: No files provided"
    pure OtherError
  Merge outputFile allFiles ->
    mergeFiles outputFile allFiles
