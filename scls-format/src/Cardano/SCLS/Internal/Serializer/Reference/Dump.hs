{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.SCLS.Internal.Serializer.Reference.Dump (
  DataStream (..),
  InputChunk,
  SerializationPlan,
  mkSortedSerializationPlan,
  defaultSerializationPlan,
  addChunks,
  withChunkFormat,
  addMetadata,
  dumpToHandle,
  constructChunks_,
  withBufferSize,
) where

import Cardano.SCLS.Internal.Frame
import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Record.Manifest
import Cardano.SCLS.Internal.Record.Metadata
import Cardano.SCLS.Internal.Serializer.ChunksBuilder.InMemory qualified as CB
import Cardano.SCLS.Internal.Serializer.MemPack
import Cardano.SCLS.Internal.Serializer.MetadataBuilder.InMemory qualified as MB
import Crypto.Hash.MerkleTree.Incremental qualified as MT

import Data.Foldable qualified as F
import Data.Function ((&))
import Data.Map (Map)
import Data.Map.Strict qualified as Map

import Cardano.Types.Namespace (Namespace)
import Data.MemPack
import Data.MemPack.Buffer (pinnedByteArrayToByteString)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Streaming (Of (..), liftIO)
import Streaming qualified as S
import Streaming.Internal (Stream (..))
import Streaming.Prelude qualified as S
import System.IO (Handle)

type InputChunk a = S.Of Namespace (S.Stream (S.Of a) IO ())

{- | A stream of values grouped by namespace.

Each element of the outer stream is a pair of:
  * a 'Text' namespace identifier
  * a stream of values of type @a@ belonging to that namespace.

Constraints:
  * Each namespace appears at most once in the stream (no duplicate namespaces).
  * The namespaces are ordered as they appear in the stream; no global ordering is enforced.
  * The values within each namespace stream are ordered as they appear.
  * Multiple segments per namespace are NOT allowed; all values for a namespace must be in a single contiguous stream.
  * The stream may be empty.

This type is used as input to chunked serialization routines, which expect the data to be grouped and ordered as described.
-}
newtype DataStream a = DataStream {runDataStream :: Stream (Of (InputChunk a)) IO ()}

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

{- | Create a serialization plan with default options and no data.
    The default options are:
    Chunk format: Raw (no compression)
    Buffer size: 16 MB
-}
defaultSerializationPlan :: forall a. (MemPack a, Typeable a) => SerializationPlan a
defaultSerializationPlan =
  SerializationPlan
    { pChunkFormat = ChunkFormatRaw
    , pBufferSize = 16 * 1024 * 1024 -- 16 MB buffer size
    , pChunkStream = mempty
    , pMetadataStream = Nothing
    }

-- | Add a chunked data stream to the serialization plan.
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

withBufferSize :: Int -> SerializationPlan a -> SerializationPlan a
withBufferSize size plan =
  plan
    { pBufferSize = size
    }

-- Dumps data to the handle, while splitting it into chunks.
--
-- This is reference implementation and it does not yet care about
-- proper working with the hardware, i.e. flushing and calling fsync
-- at the right moments.
dumpToHandle :: (MemPack a, Typeable a, MemPackHeaderOffset a) => Handle -> Hdr -> SortedSerializationPlan a -> IO ()
dumpToHandle handle hdr plan = do
  let SerializationPlan{..} = getSerializationPlan plan
  _ <- hWriteFrame handle hdr
  manifestData <-
    pChunkStream -- output our sorted stream
      & S.mapM
        ( \(namespace :> inner) -> do
            inner
              & constructChunks_ pChunkFormat pBufferSize -- compose entries into data for chunks records, returns digest of entries
              & S.copy
              & storeToHandle namespace -- stores data to handle,passes digest of entries
              & S.map CB.chunkItemEntriesCount -- keep only number of entries (other things are not needed)
              & S.copy
              & S.length -- returns number of chunks
              & S.sum -- returns number of entries
              & fmap (namespace,)
        )
      & S.fold_
        do
          \rest (namespace, (entries :> (chunks :> rootHash))) ->
            let ni =
                  NamespaceInfo
                    { namespaceEntries = fromIntegral entries
                    , namespaceChunks = fromIntegral chunks
                    , namespaceHash = rootHash
                    }
             in Map.insert namespace ni rest
        mempty
        ManifestInfo

  case pMetadataStream of
    Nothing -> pure ()
    Just s -> do
      (_entries :> (_metadataRecords :> _rootHash)) <-
        s
          & constructMetadata_ pBufferSize -- compose entries into data for metadata records, returns digest of entries
          & S.copy
          & S.map metadataToRecord
          & S.mapM_ (liftIO . hWriteFrame handle)
          & S.map MB.metadataItemEntriesCount -- keep only number of entries (other things are not needed)
          & S.copy
          & S.length -- returns number of metadata
          & S.sum -- returns number of entries
      pure ()

  manifest <- mkManifest manifestData
  _ <- hWriteFrame handle manifest
  pure ()
 where
  storeToHandle :: (S.MonadIO io) => Namespace -> Stream (Of CB.ChunkItem) io r -> io r
  storeToHandle namespace s =
    s
      & S.zip (S.enumFrom 1)
      & S.map (chunkToRecord namespace)
      & S.mapM_ (liftIO . hWriteFrame handle)

  chunkToRecord :: Namespace -> (Word64, CB.ChunkItem) -> Chunk
  chunkToRecord namespace (seqno, CB.ChunkItem{..}) =
    mkChunk
      seqno
      chunkItemFormat
      namespace
      (pinnedByteArrayToByteString chunkItemData)
      (fromIntegral chunkItemEntriesCount)

metadataToRecord :: MB.MetadataItem -> Metadata
metadataToRecord MB.MetadataItem{..} =
  mkMetadata
    (pinnedByteArrayToByteString metadataItemData)
    (fromIntegral metadataItemEntriesCount)

constructChunks_ ::
  forall a r.
  (MemPack a, Typeable a, MemPackHeaderOffset a) =>
  ChunkFormat ->
  Int ->
  Stream (Of a) IO r ->
  Stream (Of CB.ChunkItem) IO (Digest)
constructChunks_ format bufferSize s0 = liftIO initialize >>= consume s0
 where
  initialize = CB.mkMachine bufferSize format
  consume ::
    Stream (Of a) IO r ->
    CB.BuilderMachine ->
    Stream (Of CB.ChunkItem) IO (Digest)
  consume s1 !machine = do
    case s1 of
      Return{} ->
        liftIO (CB.interpretCommand machine CB.Finalize) >>= \case
          (digest, Nothing) -> return digest
          (digest, Just e) -> S.yield e >> return digest
      Effect e -> Effect (e >>= \s -> return (consume s machine))
      Step (u :> rest) -> do
        liftIO (CB.interpretCommand machine (CB.Append u)) >>= \case
          (machine', chunks) -> do
            S.each chunks
            consume rest machine'

constructMetadata_ ::
  forall r.
  Int ->
  Stream (Of MetadataEntry) IO r ->
  Stream (Of MB.MetadataItem) IO (Digest)
constructMetadata_ bufferSize s0 = liftIO initialize >>= consume s0
 where
  initialize = MB.mkMachine bufferSize
  consume ::
    Stream (Of MetadataEntry) IO r ->
    MB.BuilderMachine ->
    Stream (Of MB.MetadataItem) IO (Digest)
  consume s1 machine = do
    case s1 of
      Return{} ->
        liftIO (MB.interpretCommand machine MB.Finalize) >>= \case
          (digest, Nothing) -> return digest
          (digest, Just e) -> S.yield e >> return digest
      Effect e -> Effect (e >>= \s -> return (consume s machine))
      Step (u :> rest) -> do
        liftIO (MB.interpretCommand machine (MB.Append u)) >>= \case
          (machine', metadata) -> do
            S.each metadata
            consume rest machine'

data ManifestInfo = ManifestInfo
  { _namespaceInfo :: Map Namespace NamespaceInfo
  }

instance Semigroup ManifestInfo where
  (ManifestInfo a) <> (ManifestInfo b) = ManifestInfo (Map.union a b)

instance Monoid ManifestInfo where
  mempty = ManifestInfo Map.empty

mkManifest :: ManifestInfo -> IO Manifest
mkManifest (ManifestInfo namespaceInfo) = do
  let ns = Map.toList namespaceInfo
      totalEntries = F.foldl' (+) 0 (namespaceEntries . snd <$> ns)
      totalChunks = F.foldl' (+) 0 (namespaceChunks . snd <$> ns)
      rootHash =
        Digest $
          MT.merkleRootHash $
            MT.finalize $
              F.foldl' MT.add (MT.empty undefined) (namespaceHash . snd <$> ns)
  pure
    Manifest
      { totalEntries
      , totalChunks
      , rootHash = rootHash
      , nsInfo = namespaceInfo
      , prevManifestOffset = 0 -- TODO: support chaining of manifests
      , summary =
          ManifestSummary
            { createdAt = T.pack "2025-01-01T00:00:00Z" -- TODO: use current time
            , tool = T.pack "scls-tool:reference" -- TODO: add version (?)
            , comment = Nothing -- TODO: allow configuration
            }
      }
