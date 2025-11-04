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
import Cardano.Types.ByteOrdered (BigEndian (..))
import Control.Applicative (Alternative (many))
import Control.Monad.Fix (fix)
import Control.Monad.Trans.Fail (errorFail)
import Data.Binary (Binary (get), Word32, put)
import Data.Binary.Get (getByteString, getLazyByteString, getWord32be, getWord64be, getWord8, runGet)
import Data.Binary.Put (putByteString, putWord32be, putWord64be)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.MemPack
import Data.Word (Word64)

data MetadataEntry = MetadataEntry
  { subject :: BS.ByteString -- Should be a URI
  , value :: BS.ByteString -- CBOR encoded value
  }
  deriving (Show, Eq, Ord)

instance MemPack MetadataEntry where
  packedByteCount MetadataEntry{..} =
    4 + 4 + BS.length subject + BS.length value

  packM MetadataEntry{..} = do
    packM (BigEndian (fromIntegral $ BS.length subject :: Word32))
    packM (BigEndian (fromIntegral $ BS.length value :: Word32))
    packByteStringM subject
    packByteStringM value

  unpackM = do
    BigEndian (subjectLen :: Word32) <- unpackM
    BigEndian (valueLen :: Word32) <- unpackM
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

instance Binary MetadataFooter where
  put MetadataFooter{..} = do
    putWord64be totalEntries
    put entriesHash

  get = do
    totalEntries <- getWord64be
    entriesHash <- get
    pure MetadataFooter{..}

data Metadata = Metadata
  { metadataEntries :: [MetadataEntry]
  , metadataFooter :: MetadataFooter
  }
  deriving (Show, Eq)

instance IsFrameRecord 0x31 Metadata where
  encodeRecordContents Metadata{..} = do
    for_ metadataEntries putEntry
    put metadataFooter
   where
    putEntry MetadataEntry{..} = do
      let subjectLen = BS.length subject
          valueLen = BS.length value

      putWord32be (fromIntegral subjectLen)
      putWord32be (fromIntegral valueLen)
      putByteString subject
      putByteString value

  decodeRecordContents size = do
    _ <- getWord8 -- type offset: maintain consistency with Chunk pattern
    let entriesSize = fromIntegral size - 1 - 8 - hashDigestSize
    entriesBytes <- getLazyByteString (fromIntegral entriesSize)
    let metadataEntries = runGet (many getEntry) entriesBytes
    metadataFooter <- get
    pure Metadata{..}
   where
    getEntry = do
      subjectLen <- getWord32be
      valueLen <- getWord32be
      subject <- getByteString (fromIntegral subjectLen)
      value <- getByteString (fromIntegral valueLen)
      pure (MetadataEntry{..})

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
