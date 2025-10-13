{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.SCLS.Util.Verify (verifyRoot, verifyNamespace) where

import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Internal.Serializer.MemPack
import Cardano.SCLS.Util.Result
import Control.Exception (SomeException, catch)
import Crypto.Hash (Blake2b_224)
import Crypto.Hash.MerkleTree.Incremental qualified as MT
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Streaming.Prelude qualified as S

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
