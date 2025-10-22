{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Internal.Reader (
  withHeader,
  withNamespacedData,
  withLatestManifestFrame,
  withRecordData,
  extractRootHash,
  extractNamespaceList,
  extractNamespaceHash,
) where

import Cardano.SCLS.Internal.Frame
import Cardano.SCLS.Internal.Hash (Digest)
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Record.Manifest
import Cardano.SCLS.Internal.Serializer.MemPack
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.Trans.Fail
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.Function (fix)
import Data.Map.Strict qualified as Map
import Data.MemPack (MemPack, unpackLeftOver)
import Data.Typeable
import System.IO
import System.IO qualified as IO

import Cardano.Types.Namespace (Namespace)
import Cardano.SCLS.Internal.Record.Internal.Class (IsFrameRecord)
import Streaming qualified as S
import Streaming.Prelude qualified as S

-- | Stream all data chunks for the given namespace.
withNamespacedData :: (MemPack u, Typeable u) => FilePath -> Namespace -> (S.Stream (S.Of u) IO () -> IO a) -> IO a
withNamespacedData filePath namespace f =
  IO.withBinaryFile filePath ReadMode \handle -> f (stream handle)
 where
  stream handle = do
    flip fix headerOffset \go record -> do
      next <- S.liftIO do
        fetchNextFrame handle record
      for_ next \next_record -> do
        dataRecord <- S.liftIO do
          fetchOffsetFrame handle next_record
        for_ (decodeFrame dataRecord) \chunkRecord -> do
          when (chunkNamespace (frameViewContent (chunkRecord)) == namespace) do
            flip fix (chunkData $ frameViewContent chunkRecord) \drain -> \case
              bs
                | BS.null bs -> pure ()
                | otherwise -> do
                    let (Entry userData, off) = errorFail $ unpackLeftOver bs
                        rest = BS.drop off bs
                    S.yield userData
                    drain rest
        go next_record

{- | Stream all records for a particular record type in the file.
No optimized access, just a full scan of the file.
-}
withRecordData :: forall t b r. (IsFrameRecord t b) => FilePath -> (S.Stream (S.Of b) IO () -> IO r) -> IO r
withRecordData filePath f =
  IO.withBinaryFile filePath ReadMode \handle -> f (stream handle)
 where
  stream handle = do
    flip fix headerOffset \go record -> do
      next <- S.liftIO do
        fetchNextFrame handle record
      for_ next \next_record -> do
        dataRecord <- S.liftIO do
          fetchOffsetFrame handle next_record
        for_ (decodeFrame dataRecord) \metadataRecord -> do
          S.yield (frameViewContent metadataRecord)
        go next_record

{- | Extract the root hash from the file at the given offset.

This function does not provide additional checks.
-}
extractRootHash :: FilePath -> IO Digest
extractRootHash = withLatestManifestFrame \Manifest{..} ->
  pure rootHash

extractNamespaceHash :: Namespace -> FilePath -> IO (Maybe Digest)
extractNamespaceHash ns = withLatestManifestFrame \Manifest{..} ->
  pure (namespaceHash <$> Map.lookup ns nsInfo)

data NotSCLSFile = NotSCLSFile
  deriving (Show, Typeable, Exception)

withLatestManifestFrame :: (Manifest -> IO r) -> FilePath -> IO r
withLatestManifestFrame f filePath = do
  IO.withBinaryFile filePath ReadMode \handle -> do
    h <- hFileSize handle
    hSeek handle AbsoluteSeek (h - 4)
    bs <- BS.hGet handle 4
    offset <- case runGetOrFail getWord32be (BSL.fromStrict bs) of
      Right (_, _, d) -> return d
      Left{} -> throwIO NotSCLSFile
    frameData <- fetchOffsetFrame handle (FrameView (offset) (mkRecordType @Manifest) (fromIntegral h - fromIntegral offset))
    case decodeFrame frameData of
      Just FrameView{frameViewContent = m@Manifest{}} -> f m
      Nothing -> throwIO NotSCLSFile

extractNamespaceList :: FilePath -> IO [Namespace]
extractNamespaceList = withLatestManifestFrame \Manifest{..} ->
  pure (Map.keys nsInfo)

withHeader :: FilePath -> (Hdr -> IO r) -> IO r
withHeader filePath f = do
  IO.withBinaryFile filePath ReadMode \handle -> do
    hSeek handle AbsoluteSeek 0
    bs <- BS.hGet handle 4
    offset <- case runGetOrFail getWord32be (BSL.fromStrict bs) of
      Right (_, _, d) -> return d
      Left{} -> throwIO NotSCLSFile
    frameData <- fetchOffsetFrame handle (FrameView offset (mkRecordType @Hdr) 4)
    case decodeFrame frameData of
      Just FrameView{frameViewContent = hdr@Hdr{}} -> f hdr
      Nothing -> error "Failed to decode header"
