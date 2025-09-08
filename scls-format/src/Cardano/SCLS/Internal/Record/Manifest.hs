-- |
-- Manifest record for file integrity and summary information.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Cardano.SCLS.Internal.Record.Manifest
  ( Manifest(..)
  , ManifestSummary(..)
  ) where

import Data.Word
import Data.Binary.Get (getWord8, getWord64be, getByteString, getWord32be)
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Function (fix)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as T

import Cardano.SCLS.Internal.Record.Internal.Class

-- | Manifest summary information
data ManifestSummary = ManifestSummary
  { createdAt :: Text -- ^ ISO 8601 date string
  , tool :: Text          -- ^ tool info
  , comment :: Maybe Text -- ^ optional comment
  } deriving (Show)

-- | Manifest record
--
data Manifest = Manifest
  { totalEntries :: Word64 -- ^ number of entries
  , totalChunks :: Word64 -- ^ number of chunks
  , rootHash :: ByteString -- ^ multihash of root entry
  , nsRoots :: Map.Map Text ByteString -- ^ map from namespace to multihash
  , prevManifestOffset :: Word64 -- ^ offset of previous manifest
  , summary :: ManifestSummary -- ^ summary information
  } deriving (Show)

instance IsFrameRecord 0x01 Manifest where
  encodeRecordContents Manifest{..} = do
    putWord64be totalEntries
    putWord64be totalChunks
    encodeSummary summary
    traverse_ putNsRoot (Map.toList nsRoots)
    putWord32be 0
    putWord64be prevManifestOffset
    putByteString rootHash
    where
      putNsRoot (ns, h) = do
        let nsBytes = T.encodeUtf8 ns
        putWord32be (fromIntegral $ BS.length nsBytes)
        putByteString nsBytes
        putByteString h -- 28 bytes
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
    (Map.fromList -> nsRoots)
      <- flip fix [] $ \next current -> do
            getNsRoot >>= \case
              Nothing -> pure current
              Just n -> next (n:current)
    prevManifestOffset <- getWord64be
    rootHash <- getByteString 28
    pure Manifest{..}
    where
      getNsRoot = do
        nsLen <- getWord32be
        if nsLen == 0
        then return Nothing
        else do
          ns <- T.decodeUtf8 <$> getByteString (fromIntegral nsLen)
          h <- getByteString 28
          pure (Just (ns, h))
      decodeSummary = do
        createdAtLen <- getWord32be
        createdAt <- T.decodeUtf8 <$> getByteString (fromIntegral createdAtLen)
        toolLen <- getWord32be
        tool <- T.decodeUtf8 <$> getByteString (fromIntegral toolLen)
        comment
          <- do comment_len <- getWord32be
                if comment_len == 0
                then return Nothing
                else do c <- T.decodeUtf8 <$> getByteString (fromIntegral comment_len)
                        pure (Just c)
        pure ManifestSummary{..}