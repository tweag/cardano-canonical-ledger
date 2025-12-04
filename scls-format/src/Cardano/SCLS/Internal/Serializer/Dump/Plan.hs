{-# LANGUAGE RecordWildCards #-}

{- | Plan for running serialization.

SerializationPlan explains how to construct the values that should be serialized to
the dump file.
-}
module Cardano.SCLS.Internal.Serializer.Dump.Plan (
  -- * Plan
  SerializationPlan (..),
  InputChunk,
  ChunkStream,

  -- ** Construction
  defaultSerializationPlan,
  addNamespacedChunks,
  addChunks,
  withChunkFormat,
  addMetadata,
  withBufferSize,
  withManifestComment,
  withTimestamp,

  -- * Sorted plan
  SortedSerializationPlan (..),
  mkSortedSerializationPlan,
) where

import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry, SomeChunkEntry (SomeChunkEntry), encodeChunkEntry)
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Record.Metadata
import Cardano.SCLS.NamespaceCodec (KnownNamespace (..))
import Cardano.Types.Namespace (Namespace, fromSymbol)

import Data.MemPack (MemPack)
import Data.MemPack.Extra (RawBytes)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Proxy, Typeable)
import GHC.TypeLits (KnownSymbol)
import Streaming (Of (..))
import Streaming qualified as S
import Streaming.Internal (Stream (..))
import Streaming.Prelude qualified as S

{- | Helper to define an input data.
Each chunk is a stream of values that will be written under a given namespace.
-}
type InputChunk a = S.Of Namespace (S.Stream (S.Of a) IO ())

type ChunkStream a = Stream (Of (InputChunk a)) IO ()

type MetadataStream = Stream (Of MetadataEntry) IO ()

-- | Serialization plan with data sources and configuration options.
data SerializationPlan a = SerializationPlan
  -- Future fields for more dump configurations can be added here
  -- e.g. isToBuildIndex, deltaStream, etc.
  { pChunkFormat :: ChunkFormat
  -- ^ Compression format for chunks
  , pBufferSize :: Int
  -- ^ Buffer size for record building (in bytes)
  , pChunkStream :: ChunkStream a
  -- ^ Input stream of entries to serialize, can be unsorted
  , pMetadataStream :: Maybe MetadataStream
  -- ^ Optional stream of metadata records to include in the dump
  , pManifestComment :: Maybe Text
  -- ^ Optional comment to inlude in the file manifest
  , pTimestamp :: Maybe (UTCTime)
  -- ^ Optional timestamp value to include in the manifest. Defaults to the timestamp at the time of serialization.
  }

-- | Create a serialization plan with default options and no data.
defaultSerializationPlan :: SerializationPlan a
defaultSerializationPlan =
  SerializationPlan
    { pChunkFormat = ChunkFormatRaw
    , pBufferSize = 16 * 1024 * 1024 -- 16 MB buffer size
    , pChunkStream = mempty
    , pMetadataStream = Nothing
    , pManifestComment = Nothing
    , pTimestamp = Nothing
    }

-- | Add a chunked data stream to the dump configuration.
addNamespacedChunks ::
  forall ns.
  (KnownSymbol ns, KnownNamespace ns) =>
  Proxy ns ->
  Stream (Of (ChunkEntry (NamespaceKey ns) (NamespaceEntry ns))) IO () ->
  SerializationPlan (SomeChunkEntry RawBytes) ->
  SerializationPlan (SomeChunkEntry RawBytes)
addNamespacedChunks p stream =
  addChunks $
    S.yield
      ((fromSymbol p) :> S.map (SomeChunkEntry . encodeChunkEntry p) stream)

addChunks :: (MemPack a, Typeable a) => ChunkStream a -> SerializationPlan a -> SerializationPlan a
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
addMetadata :: MetadataStream -> SerializationPlan a -> SerializationPlan a
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

-- | Set the manifest comment value in the serialization plan.
withManifestComment :: Text -> SerializationPlan a -> SerializationPlan a
withManifestComment comment plan =
  plan
    { pManifestComment = Just comment
    }

-- | Set the timestamp value in the serialization plan.
withTimestamp :: UTCTime -> SerializationPlan a -> SerializationPlan a
withTimestamp timestamp plan =
  plan
    { pTimestamp = Just timestamp
    }

-- | A serialization plan with sorted streams.
newtype SortedSerializationPlan a = SortedSerializationPlan {getSerializationPlan :: SerializationPlan a}

{- | A function type used to sort streams.
This type alias represents a function that takes a stream and produces a sorted stream of elements.
Elements of type 'a' may be transformed into elements of type 'b' in the output stream.
-}
type SortF a b =
  (Stream (Of a) IO ()) ->
  (Stream (Of b) IO ())

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
