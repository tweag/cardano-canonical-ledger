{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{- |

Reference implementation for serializer; it's intended to be as simple
as possible.
-}
module Cardano.SCLS.Internal.Serializer.Reference (
  serialize,
) where

import Cardano.SCLS.Internal.Frame
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Record.Manifest
import Cardano.SCLS.Internal.Serializer.ChunksBuilder.InMemory
import Cardano.Types.Network
import Cardano.Types.SlotNo
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ST
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.MemPack
import Data.MemPack.Buffer
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable
import Data.Vector qualified as V
import Data.Vector.Algorithms.Tim qualified as Tim
import Data.Word
import Streaming (Of ((:>)), Stream)
import Streaming qualified as S
import Streaming.Internal (Stream (..))
import Streaming.Prelude qualified as S
import System.IO
import VectorBuilder.Builder qualified as Builder
import VectorBuilder.MVector qualified as Builder

{- | Reference serialization interface. Performs all operations in memory

At this point it accepts values from one namespace only.
-}
serialize ::
  (Foldable f, MemPack a, Ord a, Typeable a) =>
  -- | path to resulting file
  FilePath ->
  -- | Network identifier
  NetworkId ->
  -- | Slot of the current transaction
  SlotNo ->
  -- | Namespace for the data entries
  Text ->
  -- | Input stream of entries to serialize, can be unsorted
  f a ->
  IO ()
serialize resultFilePath network slotNo namespace stream = do
  withFile resultFilePath WriteMode $ \handle -> do
    dumpToHandle handle
 where
  -- First we need to sort the input value, because we do not
  -- require input to be sorted. In the real implementation we
  -- would sort only a small chunks of the input and store in to
  -- the temporary files, then merge sort them.
  orderedStream = runST $ do
    mv <- Builder.build (Builder.foldable stream)
    Tim.sort mv
    V.unsafeFreeze mv

  -- Dumps data to the handle, while splitting it into chunks.
  --
  -- This is reference implementation and it does not yet care about
  -- proper working with the hardware, i.e. flushing and calling fsync
  -- at the right moments.
  dumpToHandle :: Handle -> IO ()
  dumpToHandle handle = do
    _ <- hWriteFrame handle $! mkHdr network slotNo
    manifestData :: Of Int (Of Int ByteString) <-
      S.each orderedStream -- output our sorted stream
        & constructChunks_ -- compose entries into data for chunks records, returns digest of entries
        & S.copy
        & storeToHandle handle -- stores data to handle,passes digest of entries
        & S.map chunkItemEntriesCount -- keep only number of entries (other things are not needed)
        & S.copy
        & S.length -- returns number of chunks
        & S.sum -- returns number of entries
    manifest <- mkManifest namespace manifestData
    _ <- hWriteFrame handle manifest
    pure ()

  storeToHandle :: (S.MonadIO io) => Handle -> Stream (Of ChunkItem) io r -> io r
  storeToHandle handle s =
    s
      & S.zip (S.enumFrom 1)
      & S.map chunkToRecord
      & S.mapM_ (liftIO . hWriteFrame handle)

  chunkToRecord :: (Word64, ChunkItem) -> Chunk
  chunkToRecord (seqno, ChunkItem{..}) =
    mkChunk
      seqno
      chunkItemFormat
      namespace
      (pinnedByteArrayToByteString chunkItemData)
      (fromIntegral chunkItemEntriesCount)

mkManifest :: Text -> Of Int (Of Int ByteString) -> IO Manifest
mkManifest namespace ((fromIntegral -> totalEntries) S.:> ((fromIntegral -> totalChunks) S.:> rootHash)) = do
  pure
    Manifest
      { totalEntries
      , totalChunks
      , rootHash = rootHash
      , nsRoots = Map.fromList [(namespace, rootHash)]
      , prevManifestOffset = 0 -- TODO: support chaining of manifests
      , summary =
          ManifestSummary
            { createdAt = T.pack "2025-01-01T00:00:00Z" -- TODO: use current time
            , tool = T.pack "scls-tool:reference" -- TODO: add version (?)
            , comment = Nothing -- TODO: allow configuration
            }
      }

constructChunks_ ::
  forall a r.
  (MemPack a, Typeable a) =>
  Stream (Of a) IO r ->
  Stream (Of ChunkItem) IO ByteString
constructChunks_ s0 = liftIO initialize >>= consume s0
 where
  initialize = mkMachine (16 * 1024 * 1024) ChunkFormatRaw -- TODO: allow configuration of buffer size and format
  consume ::
    Stream (Of a) IO r ->
    BuilderMachine ->
    Stream (Of ChunkItem) IO ByteString
  consume s1 machine = do
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
