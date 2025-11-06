{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Record to hold metadata information.
-}
module Cardano.SCLS.Internal.Record.Metadata (Metadata (..), MetadataEntry (..), mkMetadata) where

import Cardano.SCLS.Internal.Hash (Digest, digest, hashDigestSize)
import Cardano.SCLS.Internal.Record.Internal.Class
import Cardano.SCLS.Internal.Serializer.MemPack
import Cardano.Types.ByteOrdered (packWord32beM, packWord64beM, unpackBigEndianM)

import Control.Monad.Fix (fix)
import Control.Monad.Trans.Fail (errorFail)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.MemPack
import Data.Word (Word32, Word64)

data MetadataEntry = MetadataEntry
  { subject :: BS.ByteString -- Should be a URI
  , value :: BS.ByteString -- CBOR encoded value
  }
  deriving (Show, Eq, Ord)

instance MemPack MetadataEntry where
  packedByteCount MetadataEntry{..} =
    4 + 4 + BS.length subject + BS.length value

  packM MetadataEntry{..} = do
    let subjectLen = BS.length subject
        valueLen = BS.length value

    packWord32beM (fromIntegral subjectLen)
    packWord32beM (fromIntegral valueLen)
    packByteStringM subject
    packByteStringM value

  unpackM = do
    subjectLen :: Word32 <- unpackBigEndianM
    valueLen :: Word32 <- unpackBigEndianM
    subject <- unpackByteStringM (fromIntegral subjectLen)
    value <- unpackByteStringM (fromIntegral valueLen)
    return MetadataEntry{..}

instance MemPackHeaderOffset MetadataEntry where
  headerSizeOffset = 8

data MetadataFooter = MetadataFooter
  { totalEntries :: !Word64
  , entriesHash :: !Digest
  }
  deriving (Show, Eq)

instance MemPack MetadataFooter where
  packedByteCount _ = 8 + hashDigestSize

  packM MetadataFooter{..} = do
    packWord64beM totalEntries
    packM entriesHash

  unpackM = do
    totalEntries <- unpackBigEndianM
    entriesHash <- unpackM
    pure MetadataFooter{..}

data Metadata = Metadata
  { metadataEntries :: [MetadataEntry]
  , metadataFooter :: MetadataFooter
  }
  deriving (Show, Eq)

instance IsFrameRecord 0x31 Metadata where
  frameRecordSize Metadata{..} =
    sum (map packedByteCount metadataEntries) + packedByteCount metadataFooter

  encodeRecordContents Metadata{..} = do
    for_ metadataEntries packM
    packM metadataFooter

  decodeRecordContents size = do
    let entriesSize = fromIntegral size - 1 - 8 - hashDigestSize
    entriesBytes <- unpackByteStringM entriesSize
    let metadataEntries =
          fix
            ( \rec bs ->
                if BS.null bs
                  then []
                  else
                    let (e, n) = errorFail $ unpackLeftOver bs
                     in e : rec (BS.drop n bs)
            )
            entriesBytes

    metadataFooter <- unpackM
    pure Metadata{..}

mkMetadata :: BS.ByteString -> Word64 -> Metadata
mkMetadata metadataBytes totalEntries = do
  let
    entriesHash = digest metadataBytes
    metadataFooter = MetadataFooter{..}
    metadataEntries =
      fix
        ( \rec bs ->
            if BS.null bs
              then []
              else
                let (Entry e, n) = errorFail $ unpackLeftOver bs
                 in e : rec (BS.drop n bs)
        )
        metadataBytes

  Metadata{..}
