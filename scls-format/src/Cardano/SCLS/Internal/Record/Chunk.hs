{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

{- |

A chunk of the actual data that is stored in the file.
In order to efficiently store and retrieve data chunk is split into
header and footer parts, so we can access them without loading the entire contents of the chunk.
-}
module Cardano.SCLS.Internal.Record.Chunk (
  Chunk (..),
  ChunkFormat (..),
  DebugChunk (..),
  mkChunk,
) where

import Data.Binary (Binary (..))
import Data.Binary.Get (getByteString, getWord32be, getWord64be, getWord8)
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding  as T
import Data.Word (Word32, Word64)
import qualified Crypto.Hash.BLAKE2.BLAKE2bp as Blake2

import Cardano.SCLS.Internal.Record.Internal.Class

data ChunkFormat
  = -- | Entries are stored uncompressed
    ChunkFormatRaw
  | -- | Chunk is compressed with seekable zstd
    ChunkFormatZstd
  | -- | Entries are individually compressed with seekable zstd
    ChunkFormatZstdE
  deriving (Show)

instance Binary ChunkFormat where
  put ChunkFormatRaw = putWord8 0
  put ChunkFormatZstd = putWord8 1
  put ChunkFormatZstdE = putWord8 2
  get = do
    tag <- getWord8
    case tag of
      0 -> pure ChunkFormatRaw
      1 -> pure ChunkFormatZstd
      2 -> pure ChunkFormatZstdE
      _ -> fail "Unknown ChunkFormat"

-- | Data chunk, undecoded version. Loads entire data in memory.
data Chunk = Chunk
  { chunkSeq :: Word64
  , chunkFormat :: ChunkFormat
  , chunkNamespace :: Text
  , chunkData :: BS.ByteString -- Use buffer instead (?) or even values generator
  , chunkEntriesCount :: Word32
  , chunkHash :: ByteString -- 28 bytes
  }

newtype DebugChunk = DebugChunk Chunk

instance Show DebugChunk where
  show (DebugChunk Chunk{..}) =
    "Chunk{seq="
      ++ show chunkSeq
      ++ ", format="
      ++ show chunkFormat
      ++ ", namespace="
      ++ show chunkNamespace
      ++ ", data.len="
      ++ show (BS.length chunkData)
      ++ ", entries="
      ++ show chunkEntriesCount
      ++ ", hash="
      ++ show chunkHash
      ++ "}"

instance IsFrameRecord 0x10 Chunk where
  encodeRecordContents Chunk{..} = do
    putWord64be chunkSeq
    put chunkFormat
    putWord32be (fromIntegral (BS.length namespace_bytes) :: Word32)
    putByteString namespace_bytes
    -- TODO: encode data depending on its format
    putWord32be (fromIntegral (BS.length chunkData) :: Word32)
    putByteString chunkData
    putWord32be chunkEntriesCount
    putByteString chunkHash
   where
    namespace_bytes = T.encodeUtf8 chunkNamespace
  decodeRecordContents = do
    _ <- getWord8 -- type offset: TODO: it does not look sane to me!
    chunkSeq <- getWord64be
    chunkFormat <- get
    namespace_size <- getWord32be
    chunkNamespace <- T.decodeUtf8 <$> getByteString (fromIntegral namespace_size)
    -- TODO: decode data depending on its format
    data_size <- getWord32be
    chunkData <- getByteString (fromIntegral data_size)
    chunkEntriesCount <- getWord32be
    chunkHash <- getByteString 28
    pure Chunk{..}

mkChunk :: Word64 -> ChunkFormat -> Text -> BS.ByteString -> Word32 -> Chunk
mkChunk seqno format namespace chunkData entriesCount =
  Chunk
    { chunkSeq = seqno
    , chunkFormat = format
    , chunkNamespace = namespace
    , chunkData = chunkData
    , chunkEntriesCount = entriesCount
    , chunkHash = Blake2.hash 28 mempty chunkData
    }
