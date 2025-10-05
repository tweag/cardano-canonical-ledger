{-# LANGUAGE BlockArguments #-}

module Cardano.SCLS.Internal.Serializer.Reference.Impl (
  serialize,
  InputChunk,
) where

import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Record.Metadata
import Cardano.SCLS.Internal.Serializer.Reference.Dump
import Cardano.Types.Network
import Cardano.Types.SlotNo
import Control.Monad.ST (runST)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.MemPack
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Tim qualified as Tim
import Streaming.Prelude qualified as S
import System.IO (IOMode (WriteMode), withBinaryFile)
import VectorBuilder.Builder qualified as Builder
import VectorBuilder.MVector qualified as Builder

{- | Reference serialization interface. Performs all operations in memory

At this point it accepts values from one namespace only.
-}
serialize ::
  (MemPack a, Ord a, Typeable a) =>
  -- | path to resulting file
  FilePath ->
  -- | Network identifier
  NetworkId ->
  -- | Slot of the current transaction
  SlotNo ->
  -- | Input stream of entries to serialize, can be unsorted
  (S.Stream (S.Of (InputChunk a)) IO ()) ->
  {- | Input stream of metadata to serialize
  TODO: this currently assumes data is sorted
  -}
  (S.Stream (S.Of Metadata) IO ()) ->
  IO ()
serialize resultFilePath network slotNo stream metadataStream = do
  withBinaryFile resultFilePath WriteMode \handle -> do
    let hdr = mkHdr network slotNo
    !orderedStream <- mkVectors stream
    dumpToHandle handle hdr $
      newDumpConfig
        & withChunks (DataStream (S.each [n S.:> S.each v | (n, v) <- Map.toList orderedStream]))
        & withMetadata metadataStream
 where
  mkVectors :: (Ord a) => S.Stream (S.Of (InputChunk a)) IO () -> IO (Map Text (V.Vector a))
  mkVectors = do
    S.foldM_
      do
        \m (ns S.:> vecStream) -> do
          v <- mkVector vecStream
          pure $! Map.insertWith (<>) ns v m
      do pure Map.empty
      do
        traverse \builder -> pure $ runST do
          mv <- Builder.build builder
          Tim.sort mv
          V.unsafeFreeze mv

  mkVector :: (Ord a) => S.Stream (S.Of a) IO () -> IO (Builder.Builder a)
  mkVector = S.fold_
    do \x e -> x <> Builder.singleton e
    do Builder.empty
    do id
