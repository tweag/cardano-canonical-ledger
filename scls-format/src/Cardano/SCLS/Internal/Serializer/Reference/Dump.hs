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
  dumpToHandle,
  constructChunks_,
) where

import Cardano.SCLS.Internal.Frame
import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Record.Manifest
import Cardano.SCLS.Internal.Serializer.ChunksBuilder.InMemory
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
  { chunkFormat :: ChunkFormat
  -- ^ Compression format for chunks
  , chunkStream :: Stream (Of (InputChunk a)) IO ()
  -- ^ Input stream of entries to serialize, can be unsorted
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
mkSortedSerializationPlan SerializationPlan{..} sorter =
  SortedSerializationPlan $
    SerializationPlan
      { chunkFormat = chunkFormat
      , chunkStream = sortedStream
      }
 where
  sortedStream = sorter chunkStream

-- | Create a serialization plan with default options and no data.
defaultSerializationPlan :: forall a. (MemPack a, Typeable a) => SerializationPlan a
defaultSerializationPlan =
  SerializationPlan
    { chunkFormat = ChunkFormatRaw
    , chunkStream = mempty
    }

-- | Add a chunked data stream to the dump configuration.
addChunks :: (MemPack a, Typeable a) => Stream (Of (InputChunk a)) IO () -> SerializationPlan a -> SerializationPlan a
addChunks stream SerializationPlan{..} =
  SerializationPlan
    { chunkFormat = chunkFormat
    , chunkStream = chunkStream <> stream
    }

-- | Set the chunk format in the serialization plan.
withChunkFormat :: ChunkFormat -> SerializationPlan a -> SerializationPlan a
withChunkFormat format SerializationPlan{..} =
  SerializationPlan
    { chunkFormat = format
    , chunkStream = chunkStream
    }

-- Dumps data to the handle, while splitting it into chunks.
--
-- This is reference implementation and it does not yet care about
-- proper working with the hardware, i.e. flushing and calling fsync
-- at the right moments.
dumpToHandle :: (MemPack a, Typeable a) => Handle -> Hdr -> SortedSerializationPlan a -> IO ()
dumpToHandle handle hdr plan = do
  let SerializationPlan{..} = getSerializationPlan plan
  _ <- hWriteFrame handle hdr
  manifestData <-
    chunkStream -- output our sorted stream
      & S.mapM
        ( \(namespace :> inner) -> do
            inner
              & constructChunks_ chunkFormat -- compose entries into data for chunks records, returns digest of entries
              & S.copy
              & storeToHandle namespace -- stores data to handle,passes digest of entries
              & S.map chunkItemEntriesCount -- keep only number of entries (other things are not needed)
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

  manifest <- mkManifest manifestData
  _ <- hWriteFrame handle manifest
  pure ()
 where
  storeToHandle :: (S.MonadIO io) => Namespace -> Stream (Of ChunkItem) io r -> io r
  storeToHandle namespace s =
    s
      & S.zip (S.enumFrom 1)
      & S.map (chunkToRecord namespace)
      & S.mapM_ (liftIO . hWriteFrame handle)

  chunkToRecord :: Namespace -> (Word64, ChunkItem) -> Chunk
  chunkToRecord namespace (seqno, ChunkItem{..}) =
    mkChunk
      seqno
      chunkItemFormat
      namespace
      (pinnedByteArrayToByteString chunkItemData)
      (fromIntegral chunkItemEntriesCount)

constructChunks_ ::
  forall a r.
  (MemPack a, Typeable a) =>
  ChunkFormat ->
  Stream (Of a) IO r ->
  Stream (Of ChunkItem) IO (Digest)
constructChunks_ format s0 = liftIO initialize >>= consume s0
 where
  initialize = mkMachine (16 * 1024 * 1024) format
  consume ::
    Stream (Of a) IO r ->
    BuilderMachine ->
    Stream (Of ChunkItem) IO (Digest)
  consume s1 !machine = do
    case s1 of
      Return{} ->
        liftIO (interpretCommand machine Finalize) >>= \case
          (digest, Nothing) -> return digest
          (digest, Just e) -> S.yield e >> return digest
      Effect e -> Effect (e >>= \s -> return (consume s machine))
      Step (u :> rest) -> do
        liftIO (interpretCommand machine (Append u)) >>= \case
          (machine', chunks) -> do
            S.each chunks
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
