{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Manifest record for file integrity and summary information.
-}
module Cardano.SCLS.Internal.Record.Manifest (
  Manifest (..),
  ManifestSummary (..),
  NamespaceInfo (..),
) where

import Data.Binary (get, put)
import Data.Binary.Get (getByteString, getWord32be, getWord64be, getWord8)
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.Function (fix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Word

import Cardano.SCLS.Internal.Frame (frameHeaderSize)
import Cardano.SCLS.Internal.Hash
import Cardano.SCLS.Internal.Record.Internal.Class

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

data NamespaceInfo = NamespaceInfo
  { namespaceEntries :: {-# UNPACK #-} !Word64
  -- ^ number of entries
  , namespaceChunks :: {-# UNPACK #-} !Word64
  -- ^ number of chunks
  , namespaceHash :: Digest
  -- ^ multihash of root entry
  }
  deriving (Show)

-- | Manifest record
data Manifest = Manifest
  { totalEntries :: Word64
  -- ^ number of entries
  , totalChunks :: Word64
  -- ^ number of chunks
  , rootHash :: Digest
  -- ^ multihash of root entry
  , nsInfo :: Map Text NamespaceInfo
  -- ^ map from namespace to its entries, chunks and multihash
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
    summarySize <- encodeSummary summary
    (sum -> nsSize) <- traverse putNsInfo (Map.toList nsInfo)
    putWord32be 0
    putWord64be prevManifestOffset
    put rootHash
    putWord32be (fromIntegral $ 8 + 8 + summarySize + nsSize + 4 + 8 + hashDigestSize + frameHeaderSize)
   where
    putNsInfo (ns, h) = do
      let nsBytes = T.encodeUtf8 ns
          bytesLength = BS.length nsBytes
      putWord32be (fromIntegral bytesLength)
      putWord64be (namespaceEntries h)
      putWord64be (namespaceChunks h)
      putByteString nsBytes
      put (namespaceHash h)
      return (4 + 8 + 8 + bytesLength + hashDigestSize)
    encodeSummary ManifestSummary{..} = do
      let createdAtBytes = T.encodeUtf8 createdAt
          toolBytes = T.encodeUtf8 tool
          createdAtLength = BS.length createdAtBytes
          toolBytesLength = BS.length toolBytes
      putWord32be (fromIntegral createdAtLength)
      putByteString createdAtBytes
      putWord32be (fromIntegral toolBytesLength)
      putByteString toolBytes
      let cBytes = maybe BS.empty T.encodeUtf8 comment
          cBytesLength = BS.length cBytes
      putWord32be (fromIntegral cBytesLength)
      putByteString cBytes
      return (4 + createdAtLength + 4 + toolBytesLength + 4 + cBytesLength)

  decodeRecordContents = do
    _ <- getWord8
    totalEntries <- getWord64be
    totalChunks <- getWord64be
    summary <- decodeSummary
    (Map.fromList -> nsInfo) <-
      flip fix [] $ \next current -> do
        getNsRoot >>= \case
          Nothing -> pure current
          Just n -> next (n : current)
    prevManifestOffset <- getWord64be
    rootHash <- get
    pure Manifest{..}
   where
    getNsRoot = do
      nsLen <- getWord32be
      if nsLen == 0
        then return Nothing
        else do
          namespaceEntries <- getWord64be
          namespaceChunks <- getWord64be
          ns <- T.decodeUtf8 <$> getByteString (fromIntegral nsLen)
          namespaceHash <- get
          pure (Just (ns, NamespaceInfo{..}))
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
