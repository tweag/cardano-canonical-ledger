{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Record to hold metadata information.
-}
module Cardano.SCLS.Internal.Record.Metadata (Metadata, mkMetadata) where

import Cardano.SCLS.Internal.Hash (Digest, digest)
import Cardano.SCLS.Internal.Record.Internal.Class
import Data.Binary (Binary (get), Word32, put)
import Data.Binary.Get (getByteString, getWord32be, getWord64be, getWord8)
import Data.Binary.Put (putByteString, putWord32be, putWord64be)
import Data.ByteString qualified as BS
import Data.Word (Word64)

-- TODO: reintroduce MetadataEntry ?
-- data MetadataEntry = MetadataEntry
--   { subject :: URI
--   , value :: BS.ByteString -- CBOR
--   }
-- deriving (Show)

data MetadataFooter = MetadataFooter
  { totalEntries :: Word64
  , entriesHash :: Digest
  }
  deriving (Show, Eq)

data Metadata = Metadata
  { entries :: BS.ByteString -- TODO: reintroduce [MetadataEntry] ?
  , footer :: MetadataFooter
  }
  deriving (Show, Eq)

instance IsFrameRecord 0x31 Metadata where
  encodeRecordContents Metadata{..} = do
    putWord32be (fromIntegral (BS.length entries) :: Word32)
    putByteString entries
    -- putWord64be 0 -- end of entries
    putFooter footer
   where
    -- putEntry MetadataEntry{..} = do
    --   let subjectBytes = serializeURIRef' subject
    --       subjectLen = BS.length subjectBytes
    --       valueLen = BS.length value

    --   putWord64be (fromIntegral subjectLen)
    --   putByteString subjectBytes
    --   putWord64be (fromIntegral valueLen)
    --   putByteString value

    putFooter MetadataFooter{..} = do
      putWord64be totalEntries
      put entriesHash

  decodeRecordContents _ = do
    _ <- getWord8 -- type offset: maintain consistency with Chunk pattern
    entriesSize <- getWord32be
    entries <- getByteString (fromIntegral entriesSize)
    -- _ <- getWord64be -- end of entries marker
    footer <- getFooter
    pure Metadata{..}
   where
    -- getEntry = do
    --   len <- getWord64be
    --   if len == 0
    --     then return Nothing
    --     else do
    --       subjectLen <- getWord64be
    --       subjectBytes <- getByteString (fromIntegral subjectLen)
    --       let subject = case parseURI strictURIParserOptions subjectBytes of
    --             Left err -> error $ "Failed to parse URI: " ++ show err
    --             Right uri -> uri
    --       valueLen <- getWord64be
    --       value <- getByteString (fromIntegral valueLen)
    --       pure (Just MetadataEntry{..})

    getFooter = do
      totalEntries <- getWord64be
      entriesHash <- get
      pure MetadataFooter{..}

mkMetadata :: BS.ByteString -> Word64 -> Metadata
mkMetadata entries totalEntries =
  let
    -- entries = [MetadataEntry subject value | (subject, value) <- metadataEntries]
    -- totalEntries = fromIntegral $ length entries
    entriesHash = digest entries
    footer = MetadataFooter{..}
   in
    Metadata{..}

-- where

-- digestEntries entries =
--   digest $ mconcat $ map entryDigestInput entries
--  where
--   entryDigestInput MetadataEntry{..} =
--     BS.singleton 0x01 <> serializeURIRef' subject <> value
