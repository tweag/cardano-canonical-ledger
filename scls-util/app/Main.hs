{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.SCLS.Util.Check
import Cardano.SCLS.Util.Debug
import Cardano.SCLS.Util.Info
import Cardano.SCLS.Util.Result
import Cardano.SCLS.Util.Tool
import Cardano.SCLS.Util.Verify
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.Namespace qualified as Namespace
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import System.Exit (exitWith)

-- | Command-line options
data Options = Options
  { optCommand :: Command
  }

data Command
  = Verify FilePath
  | Check FilePath
  | VerifyNamespace FilePath Text
  | Info FilePath
  | ListNamespaces FilePath
  | Split FilePath FilePath
  | Merge FilePath [FilePath]
  | Extract FilePath FilePath ExtractOptions
  | Debug CommandDebug

data CommandDebug
  = GenerateDebugFile FilePath [(Namespace, Maybe Int)]

parseOptions :: Parser Options
parseOptions =
  Options
    <$> hsubparser
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
          <> command
            "extract"
            ( info
                (Extract <$> fileArg <*> (argument str (metavar "OUTPUT_FILE" <> help "Output file for extracted data")) <*> extractOptions)
                (progDesc "Extract specific data into a new SCLS file")
            )
          <> command
            "check"
            ( info
                (Check <$> fileArg)
                (progDesc "Check the integrity and validity of an SCLS file")
            )
          <> command "debug" (info (Debug <$> debugCommand) (progDesc "Debugging utilities"))
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
  extractOptions :: Parser ExtractOptions
  extractOptions =
    ExtractOptions
      <$> namespaceOption
  namespaceOption =
    optional $
      option
        parseNamespaceList
        ( long "namespaces"
            <> short 'n'
            <> metavar "NAMESPACES"
            <> help "Comma-separated list of namespaces to extract"
        )
  parseNamespaceList :: ReadM [Namespace]
  parseNamespaceList = eitherReader $ \ns ->
    Right $ map (Namespace.fromText . T.strip) (T.split (== ',') (T.pack ns))
  debugCommand :: Parser CommandDebug
  debugCommand =
    hsubparser
      ( command
          "generate"
          ( info
              (GenerateDebugFile <$> fileArg <*> namespaceEntriesOption)
              (progDesc "Generate a debug SCLS file with random data")
          )
      )
   where
    namespaceEntriesOption :: Parser [(Namespace, Maybe Int)]
    namespaceEntriesOption =
      many $
        option
          parseNamespaceEntries
          ( long "namespace"
              <> metavar "NAMESPACE[:COUNT]"
              <> help "Namespace and optional number of entries to generate, default is 16"
          )
    parseNamespaceEntries :: ReadM (Namespace, Maybe Int)
    parseNamespaceEntries = eitherReader $ \arg ->
      case T.split (== ':') (T.pack arg) of
        [ns] -> Right (Namespace.fromText (T.strip ns), Nothing)
        [ns, countText] ->
          case reads (T.unpack (T.strip countText)) :: [(Int, String)] of
            [(count, "")] -> Right (Namespace.fromText (T.strip ns), Just count)
            _ -> Left $ "Invalid count: " ++ T.unpack countText
        _ -> Left $ "Invalid namespace entry: " ++ arg

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
  VerifyNamespace file ns -> verifyNamespace file (Namespace.fromText ns)
  Info file -> displayInfo file
  ListNamespaces file -> listNamespaces file
  Split file outputDir -> splitFile file outputDir
  Merge _ [] -> do
    putStrLn "Error: No files provided"
    pure OtherError
  Merge outputFile allFiles ->
    mergeFiles outputFile allFiles
  Extract file outputDir options ->
    extract file outputDir options
  Check file -> check file
  Debug debugCmd -> case debugCmd of
    GenerateDebugFile outputFile namespaceEntries -> generateDebugFile outputFile namespaceEntries
