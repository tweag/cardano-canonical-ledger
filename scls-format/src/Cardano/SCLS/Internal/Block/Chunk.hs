-- |
--
-- A chunk of the actual data that is stored in the file.
-- In order to efficiently store and retrieve data chunk is split into
-- header and footer parts, so we can access them without loading the entire contents of the chunk.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Cardano.SCLS.Internal.Block.Chunk
  ( Chunk(..)
  , ChunkFormat(..)
  , DebugChunk(..)
  , mkChunk
  ) where

import Data.Word
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8, getWord64be, getWord32be, getByteString)
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text.Encoding qualified as T
import Data.Text (Text)

import Cardano.SCLS.Internal.Block.Internal.Class

data ChunkFormat
  = ChunkFormatRaw
  | ChunkFormatZstd
  | ChunkFormatZstdE
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
    { chunkSeq  :: Word64
    , chunkFormat :: ChunkFormat
    , chunkNamespace :: Text
    , chunkData :: BS.ByteString -- Use buffer instead (?) or even values generator
    , chunkEntriesCount :: Word32
    , chunkHash :: ByteString -- 28 bytes
    }

newtype DebugChunk = DebugChunk Chunk

instance Show DebugChunk where
  show (DebugChunk Chunk{..}) =
    "Chunk{seq=" ++ show chunkSeq ++
    ", format=" ++ show chunkFormat ++
    ", namespace=" ++ show chunkNamespace ++
    ", data.len=" ++ show (BS.length chunkData) ++
    ", entries=" ++ show chunkEntriesCount ++
    ", hash=" ++ show chunkHash ++
    "}"

instance IsFrameBlock 0x10 Chunk where
  encodeBlockContents Chunk{..} = do
    putWord64be chunkSeq
    put chunkFormat
    putWord32be (fromIntegral (BS.length namespace_bytes) :: Word32)
    putByteString namespace_bytes
    -- TODO: encode data depending on it's format
    putWord32be (fromIntegral (BS.length chunkData) :: Word32)
    putByteString chunkData
    putWord32be chunkEntriesCount
    putByteString chunkHash
    where
      namespace_bytes = T.encodeUtf8 chunkNamespace
  decodeBlockContents = do
    _ <- getWord8 -- type offset: TODO: it does not look sane to me!
    chunkSeq <- getWord64be
    chunkFormat <- get
    namespace_size <- getWord32be
    chunkNamespace <- T.decodeUtf8 <$> getByteString (fromIntegral namespace_size)
    -- TODO: decode data depending on it's format
    data_size <- getWord32be
    chunkData <- getByteString (fromIntegral data_size)
    chunkEntriesCount <- getWord32be
    chunkHash <- getByteString 28
    pure Chunk{..}

mkChunk :: Word64 -> ChunkFormat -> Text -> BS.ByteString -> Word32 -> Chunk
mkChunk seqno format namespace chunkData entriesCount = Chunk
    { chunkSeq = seqno
    , chunkFormat = format
    , chunkNamespace = namespace
    , chunkData = chunkData
    , chunkEntriesCount = entriesCount
    , chunkHash = BS.replicate (28-8) 0 <> BS8.pack "DEADF00D" -- TODO: implement hash calculation
    }