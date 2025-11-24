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
  withBufferSize,
  withManifestComment,

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
import Data.Text (Text)
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
  { pChunkFormat :: ChunkFormat
  -- ^ Compression format for chunks
  , pBufferSize :: Int
  -- ^ Buffer size for record building (in bytes)
  , pChunkStream :: Stream (Of (InputChunk a)) IO ()
  -- ^ Input stream of entries to serialize, can be unsorted
  , pMetadataStream :: Maybe (Stream (Of MetadataEntry) IO ())
  -- ^ Optional stream of metadata records to include in the dump
  , pManifestComment :: Maybe Text
  -- ^ Optional comment to inlude in the file manifest
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
    { pChunkFormat = ChunkFormatRaw
    , pBufferSize = 16 * 1024 * 1024 -- 16 MB buffer size
    , pChunkStream = mempty
    , pMetadataStream = Nothing
    , pManifestComment = Nothing
    }

-- | Add a chunked data stream to the dump configuration.
addChunks :: (MemPack a, Typeable a) => Stream (Of (InputChunk a)) IO () -> SerializationPlan a -> SerializationPlan a
addChunks stream plan@SerializationPlan{..} =
  plan
    { pChunkStream = pChunkStream <> stream
    }

-- | Set the chunk format in the serialization plan.
withChunkFormat :: ChunkFormat -> SerializationPlan a -> SerializationPlan a
withChunkFormat format plan =
  plan
    { pChunkFormat = format
    }

-- | Add a metadata stream to the serialization plan.
addMetadata :: Stream (Of MetadataEntry) IO () -> SerializationPlan a -> SerializationPlan a
addMetadata stream plan =
  plan
    { pMetadataStream = Just stream
    }

-- | Set the buffer size in the serialization plan.
withBufferSize :: Int -> SerializationPlan a -> SerializationPlan a
withBufferSize size plan =
  plan
    { pBufferSize = size
    }

withManifestComment :: Text -> SerializationPlan a -> SerializationPlan a
withManifestComment comment plan =
  plan
    { pManifestComment = Just comment
    }

-- | A serialization plan with sorted streams.
newtype SortedSerializationPlan a = SortedSerializationPlan {getSerializationPlan :: SerializationPlan a}

-- | Create a sorted serialization plan from an existing plan and sorter functions.
mkSortedSerializationPlan ::
  SerializationPlan a ->
  SortF (InputChunk a) (InputChunk b) ->
  SortedSerializationPlan b
mkSortedSerializationPlan plan@SerializationPlan{..} sorter =
  SortedSerializationPlan $
    plan
      { pChunkStream = sorter pChunkStream
      }
