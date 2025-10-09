{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Internal.Record.Manifest
import Cardano.SCLS.Internal.Serializer.MemPack
import Control.Exception (SomeException, catch)
import Crypto.Hash (Blake2b_224)
import Crypto.Hash.MerkleTree.Incremental qualified as MT
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import Streaming.Prelude qualified as S
import System.Exit (ExitCode (..), exitWith)

-- | Command-line options
data Options = Options
  { optCommand :: Command
  }

data Command
  = Verify FilePath
  | VerifyNamespace FilePath Text
  | Info FilePath
  | ListNamespaces FilePath

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
      )
 where
  fileArg =
    argument
      str
      (metavar "FILE" <> help "Path to SCLS file")
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

data Result
  = Ok
  | VerifyFailure -- TODO: add more detailed errors and mapping to exit codes
  | OtherError

toErrorCode :: Result -> ExitCode
toErrorCode = \case
  Ok -> ExitSuccess
  VerifyFailure -> ExitFailure 65
  OtherError -> ExitFailure 1

-- | Execute the selected command
runCommand :: Command -> IO Result
runCommand = \case
  Verify file -> verifyRoot file
  VerifyNamespace file ns -> verifyNamespace file ns
  Info file -> displayInfo file
  ListNamespaces file -> listNamespaces file

verifyRoot :: FilePath -> IO Result
verifyRoot filePath = do
  putStrLn $ "Verifying root hash for: " ++ filePath
  catch
    do
      storedHash <- extractRootHash filePath
      namespaces <- extractNamespaceList filePath
      computedHash <- computeRootHash filePath namespaces

      if storedHash == computedHash
        then do
          putStrLn "Root hash verification PASSED"
          putStrLn $ "Hash: " ++ show storedHash
          pure Ok
        else do
          putStrLn "Root hash verification FAILED"
          putStrLn $ "Expected: " ++ show storedHash
          putStrLn $ "Computed: " ++ show computedHash
          pure VerifyFailure
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError

computeRootHash :: FilePath -> [Text] -> IO Digest
computeRootHash filePath namespaces = do
  finalState <- foldl (combineNamespaceHash filePath) (pure $ MT.empty (undefined :: Blake2b_224)) namespaces
  pure $ Digest $ MT.merkleRootHash $ MT.finalize finalState
 where
  combineNamespaceHash :: FilePath -> IO (MT.MerkleTreeState Blake2b_224) -> Text -> IO (MT.MerkleTreeState Blake2b_224)
  combineNamespaceHash fp stateIO ns = do
    state <- stateIO
    nsHash <- computeNamespaceHash fp ns
    pure $ MT.add state nsHash

verifyNamespace :: FilePath -> Text -> IO Result
verifyNamespace filePath namespace = do
  putStrLn $ "Verifying hash for namespace: " ++ T.unpack namespace
  catch
    do
      extractNamespaceHash namespace filePath >>= \case
        Nothing -> do
          putStrLn $ "Namespace not found"
          pure VerifyFailure
        Just storedHash -> do
          computedHash <- computeNamespaceHash filePath namespace

          if storedHash == computedHash
            then do
              putStrLn $ "Namespace hash verification PASSED"
              putStrLn $ "Hash: " ++ show storedHash
              pure Ok
            else do
              putStrLn $ "Namespace hash verification FAILED"
              putStrLn $ "Expected: " ++ show storedHash
              putStrLn $ "Computed: " ++ show computedHash
              pure VerifyFailure
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError

computeNamespaceHash :: FilePath -> Text -> IO Digest
computeNamespaceHash filePath namespace = do
  withNamespacedData filePath namespace \stream -> do
    stream
      & S.fold_
        (\acc (RawBytes bs) -> MT.add acc bs)
        (MT.empty undefined)
        (Digest . MT.merkleRootHash . MT.finalize)

displayInfo :: FilePath -> IO Result
displayInfo filePath = do
  putStrLn $ "File: " ++ filePath
  catch
    do
      Manifest{..} <- withLatestManifestFrame pure filePath

      putStrLn "\n=== File Information ==="
      putStrLn $ "Root Hash: " ++ show rootHash
      putStrLn $ "Total Entries: " ++ show totalEntries
      putStrLn $ "Total Chunks: " ++ show totalChunks

      putStrLn "\n=== Namespaces ==="
      if null nsInfo
        then putStrLn "(No namespaces)"
        else do
          mapM_
            ( \(ns, NamespaceInfo{..}) -> do
                putStrLn $ "\n" ++ T.unpack ns ++ ":"
                putStrLn $ "  Hash: " ++ show namespaceHash
                putStrLn $ "  Entries: " ++ show namespaceEntries
                putStrLn $ "  Chunks: " ++ show namespaceChunks
            )
            $ Map.toList nsInfo

      pure Ok
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError

listNamespaces :: FilePath -> IO Result
listNamespaces filePath = do
  catch
    do
      namespaces <- extractNamespaceList filePath
      if null namespaces
        then putStrLn "No namespaces found"
        else do
          putStrLn "Namespaces:"
          mapM_ (putStrLn . ("  - " <>) . T.unpack) namespaces
      pure Ok
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError
