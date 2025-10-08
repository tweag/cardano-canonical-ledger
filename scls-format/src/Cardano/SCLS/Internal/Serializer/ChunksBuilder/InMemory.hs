{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Implementation of the state machine that fills current chunk in memory.

It manages proper filling of the buffers and emitting the values when
the next item can't be added.

Current implementation expects the incoming values in already sorted order.

Implementation is done in the way so it would be possible to use it with
any existing stream and effect system as long as they could carry a state.
-}
module Cardano.SCLS.Internal.Serializer.ChunksBuilder.InMemory (
  mkMachine,
  Command (..),
  BuilderMachine (..),
  ChunkItem (..),
) where

import Cardano.SCLS.Internal.Hash
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Serializer.MemPack
import Control.Monad.Primitive
import Crypto.Hash (Blake2b_224 (Blake2b_224))
import Crypto.Hash.MerkleTree.Incremental qualified as MT
import Data.MemPack

import Data.Primitive.ByteArray
import Data.Typeable
import Debug.Trace (traceM)
import Foreign.Ptr
import Unsafe.Coerce (unsafeCoerce)

data ChunkItem = ChunkItem
  { chunkItemFormat :: !ChunkFormat
  , chunkItemData :: !ByteArray
  , chunkItemEntriesCount :: !Int
  }

-- | Command for the state machine
data Command type_ where
  -- | Append a new item to the buffer.
  Append :: (MemPack u, Typeable u) => u -> Command (BuilderMachine, [ChunkItem])
  {- | Finalize building of the buffer. Calling this command does not

    It's up to the implementation if the state machine can be used
    after interpreting this command.
  -}
  Finalize :: Command (Digest, Maybe ChunkItem)

{- | State machine for building chunks in memory.

Basically it's an interpreter for the 'Command' type that is implemented
the way that it can be inserted into the different streaming pipelines
or effect libraries
-}
newtype BuilderMachine = BuilderMachine
  { interpretCommand :: forall result. Command result -> IO result
  }

-- | Create an instance of the state machine.
mkMachine ::
  -- | Buffer size in bytes
  Int ->
  -- | Encoding format in chunks
  ChunkFormat ->
  IO BuilderMachine
mkMachine _ ChunkFormatZstd = error "Chunk format zstd is not implemented yet"
mkMachine _ ChunkFormatZstdE = error "Chunk format zstd-e is not implemented yet"
mkMachine bufferSize format@ChunkFormatRaw = do
  -- We perform copying when we emit data outside of the state machine.
  -- So this buffer is reused for all the chunks, as a result we copy
  --
  -- We allocate pinned memory because we pass it to the digest code
  -- without copying by passing raw pointer.
  storage <- newPinnedByteArray bufferSize

  -- Use fix? We love fixed point combinators do we not?
  let machine (!entriesCount :: Int) (!offset :: Int) !merkleTreeState =
        BuilderMachine
          { interpretCommand = \case
              Finalize -> do
                traceM $ "Finalize machine"
                let final = Digest $ MT.merkleRootHash $ MT.finalize merkleTreeState
                print final
                if offset == 0 -- no new data, nothing to emit
                  then
                    pure (final, Nothing)
                  else do
                    frozenData <- freezeByteArrayPinned storage 0 offset
                    pure (final, Just ChunkItem{chunkItemEntriesCount = entriesCount, chunkItemFormat = format, chunkItemData = frozenData})
              Append input -> do
                traceM $ "Append entry"
                let entry = Entry input
                let l = packedByteCount entry
                if offset + l <= bufferSize -- if we fit the current buffer we just need to write data and continue
                  then do
                    (merkleTreeState', newOffset) <-
                      unsafeAppendEntryToBuffer merkleTreeState storage offset entry
                    pure (machine (entriesCount + 1) newOffset merkleTreeState', [])
                  else do
                    -- We have no space in the current buffer, so we need to emit it first
                    frozenBuffer <- freezeByteArrayPinned storage 0 offset
                    if l > bufferSize
                      then do
                        let !tmpBuffer = pack entry
                            !merkleTreeState' = MT.add merkleTreeState (uncheckedByteArrayEntryContents tmpBuffer)
                        return
                          ( machine 0 0 merkleTreeState'
                          , mkChunksToEmit [(format, frozenBuffer, entriesCount), (format, tmpBuffer, 1)]
                          )
                      else do
                        (merkleTreeState', newOffset) <-
                          unsafeAppendEntryToBuffer merkleTreeState storage 0 entry
                        pure
                          ( machine 1 newOffset merkleTreeState'
                          , mkChunksToEmit [(format, frozenBuffer, entriesCount)]
                          )
          }
  return $! machine 0 0 (MT.empty Blake2b_224)

{- | Freeze a bytearray to the pinned immutable bytearray by copying its contents.

It's safe to use the source bytearray after this operation.
-}
freezeByteArrayPinned :: (PrimMonad m) => MutableByteArray (PrimState m) -> Int -> Int -> m ByteArray
freezeByteArrayPinned !src !off !len = do
  dst <- newPinnedByteArray len
  copyMutableByteArray dst 0 src off len
  unsafeFreezeByteArray dst

unsafeAppendEntryToBuffer :: (MemPack u, Typeable u) => MT.MerkleTreeState Blake2b_224 -> MutableByteArray (PrimState IO) -> Int -> Entry u -> IO (MT.MerkleTreeState Blake2b_224, Int)
unsafeAppendEntryToBuffer !merkleTreeState !storage !offset u = do
  newOffset <- unsafeAppendToBuffer storage offset u
  let l = newOffset - offset
  merkleTreeState' <- withMutableByteArrayContents storage $ \ptr -> do
    let csb = CStringLenBuffer (ptr `plusPtr` (offset + 4), l - 4)
    return $! MT.add merkleTreeState csb
  return (merkleTreeState', newOffset)

{- | Helper to get access to the entry contents.
This method should be used on the pinned 'ByteArray' only, but the function does
not enforce this.
-}
uncheckedByteArrayEntryContents :: ByteArray -> CStringLenBuffer
uncheckedByteArrayEntryContents !buffer = CStringLenBuffer (byteArrayContents buffer `plusPtr` 4, sizeofByteArray buffer - 4)

{- | Unsafe helper that we need because MemPack interface only allows ST, and
no other PrimMonad.

There is unsafe prefix, because this function uses 'unsafeCoerce' internally,
but it ensures everything to make it safe to use.

This functions prepends the packed values with its lengths.
-}
unsafeAppendToBuffer :: (MemPack u) => MutableByteArray (PrimState IO) -> Int -> u -> IO Int
unsafeAppendToBuffer !storage !offset u = stToPrim $ do
  let uInST = unsafeCoerce storage
  (_, offset') <-
    runStateT (runPack (packM u) uInST) offset
  pure offset'

{- | Helper to create the list of chunks to emit from the list of
  (format, data, count) tuples.

  This function filters out the chunks with 0 entries.
-}
mkChunksToEmit :: [(ChunkFormat, ByteArray, Int)] -> [ChunkItem]
mkChunksToEmit = mkChunksToEmit' []
 where
  mkChunksToEmit' acc [] = reverse acc
  mkChunksToEmit' acc ((_, _, 0) : xs) = mkChunksToEmit' acc xs
  mkChunksToEmit' acc ((format, u, count) : xs) =
    mkChunksToEmit' (ChunkItem{chunkItemFormat = format, chunkItemData = u, chunkItemEntriesCount = count} : acc) xs
