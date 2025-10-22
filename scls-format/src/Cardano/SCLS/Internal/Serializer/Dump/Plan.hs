{-# LANGUAGE RecordWildCards #-}

{- | Plan for running serialization.

SerializationPlan explains how to construct the values that should be serialized to
the dump file.
-}
module Cardano.SCLS.Internal.Serializer.Dump.Plan (
  -- * Plan
  SerializationPlan (..),
  InputChunk,

  -- ** Construction
  defaultSerializationPlan,
  addChunks,
  withChunkFormat,
  addMetadata,

  -- * Sorted plan
  SortedSerializationPlan,
  getSerializationPlan,
  mkSortedSerializationPlan,
  SortF,
) where

import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Record.Metadata

import Cardano.Types.Namespace (Namespace)
import Data.MemPack
import Data.Typeable (Typeable)
import Streaming (Of (..))
import Streaming qualified as S
import Streaming.Internal (Stream (..))

{- | Helper to define an input data.
Each chunk is a stream of values that will be written under a given namespace.
-}
type InputChunk a = S.Of Namespace (S.Stream (S.Of a) IO ())

-- | Serialization plan with data sources and configuration options.
data SerializationPlan a = SerializationPlan
  -- Future fields for more dump configurations can be added here
  -- e.g. isToBuildIndex, deltaStream, etc.
  { chunkFormat :: ChunkFormat
  -- ^ Compression format for chunks
  , chunkStream :: Stream (Of (InputChunk a)) IO ()
  -- ^ Input stream of entries to serialize, can be unsorted
  , metadataStream :: Maybe (Stream (Of MetadataEntry) IO ())
  -- ^ Optional stream of metadata records to include in the dump
  }

{- | A function type used to sort streams.
This type alias represents a function that takes a stream and produces a sorted stream of elements.
Elements of type 'a' may be transformed into elements of type 'b' in the output stream.
-}
type SortF a b =
  (Stream (Of a) IO ()) ->
  (Stream (Of b) IO ())

-- | Create a serialization plan with default options and no data.
defaultSerializationPlan :: forall a. (MemPack a, Typeable a) => SerializationPlan a
defaultSerializationPlan =
  SerializationPlan
    { chunkFormat = ChunkFormatRaw
    , chunkStream = mempty
    , metadataStream = Nothing
    }

-- | Add a chunked data stream to the dump configuration.
addChunks :: (MemPack a, Typeable a) => Stream (Of (InputChunk a)) IO () -> SerializationPlan a -> SerializationPlan a
addChunks stream SerializationPlan{..} =
  SerializationPlan
    { chunkFormat = chunkFormat
    , chunkStream = chunkStream <> stream
    , metadataStream = metadataStream
    }

-- | Set the chunk format in the serialization plan.
withChunkFormat :: ChunkFormat -> SerializationPlan a -> SerializationPlan a
withChunkFormat format SerializationPlan{..} =
  SerializationPlan
    { chunkFormat = format
    , chunkStream = chunkStream
    , metadataStream = metadataStream
    }

-- | Add a metadata stream to the serialization plan.
addMetadata :: Stream (Of MetadataEntry) IO () -> SerializationPlan a -> SerializationPlan a
addMetadata stream (SerializationPlan{..}) =
  SerializationPlan
    { chunkFormat = chunkFormat
    , chunkStream = chunkStream
    , metadataStream = Just stream
    }

-- | A serialization plan with sorted streams.
newtype SortedSerializationPlan a = SortedSerializationPlan {getSerializationPlan :: SerializationPlan a}

-- | Create a sorted serialization plan from an existing plan and sorter functions.
mkSortedSerializationPlan ::
  SerializationPlan a ->
  SortF (InputChunk a) (InputChunk b) ->
  SortedSerializationPlan b
mkSortedSerializationPlan SerializationPlan{..} sorter =
  SortedSerializationPlan $
    SerializationPlan
      { chunkFormat = chunkFormat
      , chunkStream = sortedStream
      , metadataStream = metadataStream
      }
 where
  sortedStream = sorter chunkStream
