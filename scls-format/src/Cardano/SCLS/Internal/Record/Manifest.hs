{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Manifest record for file integrity and summary information.
-}
module Cardano.SCLS.Internal.Record.Manifest (
  Manifest (..),
  ManifestSummary (..),
) where

import Data.Binary.Get (getByteString, getWord32be, getWord64be, getWord8)
import Data.Binary.Put
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Function (fix)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Word

import Cardano.SCLS.Internal.Record.Internal.Class
import Crypto.Hash (Blake2b_224, HashAlgorithm (hashDigestSize), digestFromByteString)
import Crypto.Hash.MerkleTree.Incremental (MerkleHash)

-- | Manifest summary information
data ManifestSummary = ManifestSummary
  { createdAt :: Text
  -- ^ ISO 8601 date string
  , tool :: Text
  -- ^ tool info
  , comment :: Maybe Text
  -- ^ optional comment
  }
  deriving (Show)

-- | Manifest record
data Manifest = Manifest
  { totalEntries :: Word64
  -- ^ number of entries
  , totalChunks :: Word64
  -- ^ number of chunks
  , rootHash :: MerkleHash Blake2b_224
  -- ^ multihash of root entry
  , nsRoots :: Map.Map Text (MerkleHash Blake2b_224)
  -- ^ map from namespace to multihash
  , prevManifestOffset :: Word64
  -- ^ offset of previous manifest
  , summary :: ManifestSummary
  -- ^ summary information
  }
  deriving (Show)

instance IsFrameRecord 0x01 Manifest where
  encodeRecordContents Manifest{..} = do
    putWord64be totalEntries
    putWord64be totalChunks
    encodeSummary summary
    traverse_ putNsRoot (Map.toList nsRoots)
    putWord32be 0
    putWord64be prevManifestOffset
    putMerkleHash rootHash
   where
    putMerkleHash h = do
      putWord32be $ fromIntegral $ hashDigestSize (undefined :: Blake2b_224)
      putByteString $ BA.convert $ h
    putNsRoot (ns, h) = do
      let nsBytes = T.encodeUtf8 ns
      putWord32be (fromIntegral $ BS.length nsBytes)
      putByteString nsBytes
      putMerkleHash h
    encodeSummary ManifestSummary{..} = do
      let createdAtBytes = T.encodeUtf8 createdAt
          toolBytes = T.encodeUtf8 tool
      putWord32be (fromIntegral $ BS.length createdAtBytes)
      putByteString createdAtBytes
      putWord32be (fromIntegral $ BS.length toolBytes)
      putByteString toolBytes
      let cBytes = maybe BS.empty T.encodeUtf8 comment
      putWord32be (fromIntegral $ BS.length cBytes)
      putByteString cBytes

  decodeRecordContents = do
    _ <- getWord8
    totalEntries <- getWord64be
    totalChunks <- getWord64be
    summary <- decodeSummary
    (Map.fromList -> nsRoots) <-
      flip fix [] $ \next current -> do
        getNsRoot >>= \case
          Nothing -> pure current
          Just n -> next (n : current)
    prevManifestOffset <- getWord64be
    rootHash <- getMerkleHash
    pure Manifest{..}
   where
    getMerkleHash = do
      len <- getWord32be
      bs <- getByteString (fromIntegral len)
      case digestFromByteString bs of
        Nothing -> fail $ "Failed to parse MerkleHash from ByteString"
        Just h -> pure h
    getNsRoot = do
      nsLen <- getWord32be
      if nsLen == 0
        then return Nothing
        else do
          ns <- T.decodeUtf8 <$> getByteString (fromIntegral nsLen)
          h <- getMerkleHash
          pure (Just (ns, h))
    decodeSummary = do
      createdAtLen <- getWord32be
      createdAt <- T.decodeUtf8 <$> getByteString (fromIntegral createdAtLen)
      toolLen <- getWord32be
      tool <- T.decodeUtf8 <$> getByteString (fromIntegral toolLen)
      comment <-
        do
          comment_len <- getWord32be
          if comment_len == 0
            then return Nothing
            else do
              c <- T.decodeUtf8 <$> getByteString (fromIntegral comment_len)
              pure (Just c)
      pure ManifestSummary{..}
