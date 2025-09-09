{-# LANGUAGE BlockArguments #-}

module Cardano.SCLS.Internal.Serializer.Reference.Impl (
  serialize,
) where

import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Serializer.Reference.Dump
import Cardano.Types.Network
import Cardano.Types.SlotNo
import Control.Monad.ST (runST)
import Data.MemPack
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Tim qualified as Tim
import Streaming.Prelude qualified as S
import System.IO (IOMode (WriteMode), withFile)
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
  withFile resultFilePath WriteMode \handle -> do
    let hdr = mkHdr network slotNo
    let !orderedStream = runST do
          mv <- Builder.build (Builder.foldable stream)
          Tim.sort mv
          V.unsafeFreeze mv
    dumpToHandle handle namespace hdr (S.each orderedStream)
