{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.ByteString qualified as BS
import Data.Word (Word32, Word64)

import Cardano.SCLS.Internal.Hash
import Cardano.SCLS.Internal.Record.Internal.Class
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.Namespace qualified as Namespace

data ChunkFormat
  = -- | Entries are stored uncompressed
    ChunkFormatRaw
  | -- | Chunk is compressed with seekable zstd
    ChunkFormatZstd
  | -- | Entries are individually compressed with seekable zstd
    ChunkFormatZstdE
  deriving (Show, Eq)

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
  , chunkNamespace :: Namespace
  , chunkData :: BS.ByteString -- Use buffer instead (?) or even values generator
  , chunkEntriesCount :: Word32
  , chunkHash :: Digest
  }

newtype DebugChunk = DebugChunk Chunk

instance Show DebugChunk where
  show (DebugChunk Chunk{..}) =
    "Chunk{seq="
      ++ show chunkSeq
      ++ ", format="
      ++ show chunkFormat
      ++ ", namespace="
      ++ Namespace.asString chunkNamespace
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
    putByteString chunkData
    putWord32be chunkEntriesCount
    put chunkHash
   where
    namespace_bytes = Namespace.asBytes chunkNamespace
  decodeRecordContents size = do
    _ <- getWord8 -- type offset: TODO: it does not look sane to me!
    chunkSeq <- getWord64be
    chunkFormat <- get
    namespace_size <- getWord32be
    chunkNamespace <-
      (Namespace.parseBytes <$> getByteString (fromIntegral namespace_size)) >>= \case
        Left e -> fail (show e)
        Right x -> pure x
    let chunkDataSize = fromIntegral size - 1 - 8 - 1 - 4 - fromIntegral namespace_size - 4 - hashDigestSize
    chunkData <- getByteString chunkDataSize
    chunkEntriesCount <- getWord32be
    chunkHash <- get
    pure Chunk{..}

mkChunk :: Word64 -> ChunkFormat -> Namespace -> BS.ByteString -> Word32 -> Chunk
mkChunk seqno format namespace chunkData entriesCount =
  Chunk
    { chunkSeq = seqno
    , chunkFormat = format
    , chunkNamespace = namespace
    , chunkData = chunkData
    , chunkEntriesCount = entriesCount
    , chunkHash = digest chunkData
    }
