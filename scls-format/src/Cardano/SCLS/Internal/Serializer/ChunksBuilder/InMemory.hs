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

import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Serializer.MemPack
import Control.Monad.Primitive
import Crypto.Hash (Blake2b_160, Context, hashFinalize, hashInit, hashUpdate)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString.Internal (unsafePackLenLiteral)
import Data.MemPack
import Data.MemPack.Buffer as Buffer
import Data.Primitive.ByteArray
import Data.Typeable
import Foreign.Ptr
import Unsafe.Coerce (unsafeCoerce)

type Digest = Context Blake2b_160

-- Dummy implementation of the merke tree functions, this implementation
-- will be changed to the real one once merkle-tree-iterative  library will
-- be merged.
merkleInitialize :: Digest
merkleInitialize = hashInit
merkleFinalize :: Digest -> ByteString
merkleFinalize = BA.convert . hashFinalize
merkleUpdate :: (Buffer u) => Digest -> u -> Digest
merkleUpdate d u =
  Buffer.buffer
    u
    (\bytes -> hashUpdate d (pinnedByteArrayToByteString (ByteArray bytes)))
    (\addr -> hashUpdate d (unsafePackLenLiteral (bufferByteCount u) addr))

data ChunkItem = ChunkItem
  { chunkItemFormat :: ChunkFormat
  , chunkItemData :: ByteArray
  , chunkItemEntriesCount :: Int
  }

-- | Command for the state machine
data Command type_ where
  -- | Append a new item to the buffer.
  Append :: (MemPack u, Typeable u) => u -> Command (BuilderMachine, [ChunkItem])
  -- | Finalize building of the buffer. Calling this command does not
  --
  --     It's up to the implementation if the state machine can be used
  --     after interpreting this command.
  Finalize :: Command (ByteString, Maybe ChunkItem)

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
  let machine (!entriesCount :: Int) (!offset :: Int) !digest =
        BuilderMachine
          { interpretCommand = \case
              Finalize -> do
                let final = merkleFinalize digest
                if offset == 0 -- no new data, nothing to emit
                  then
                    pure (final, Nothing)
                  else do
                    frozenData <- freezeByteArrayPinned storage 0 offset
                    pure (final, Just ChunkItem{chunkItemEntriesCount = entriesCount, chunkItemFormat = format, chunkItemData = frozenData})
              Append input -> do
                let entry = Entry input
                let l = packedByteCount entry
                if offset + l <= bufferSize -- if we fit the current buffer we just need to write data and continue
                  then do
                    newOffset <- unsafeAppendToBuffer storage offset entry
                    digest' <- withMutableByteArrayContents storage $ \ptr ->
                      pure $! merkleUpdate digest (CStringLenBuffer (ptr `plusPtr` offset, l))
                    pure (machine (entriesCount + 1) newOffset digest', [])
                  else do
                    -- We have no space in the current buffer, so we need to emit it first
                    frozenBuffer <- freezeByteArrayPinned storage 0 offset
                    if l > bufferSize
                      then do
                        let tmpBuffer = pack entry
                            digest' = merkleUpdate digest tmpBuffer
                        return
                          ( machine 0 0 digest'
                          ,
                            [ ChunkItem{chunkItemFormat = format, chunkItemData = frozenBuffer, chunkItemEntriesCount = entriesCount}
                            , ChunkItem{chunkItemFormat = format, chunkItemData = tmpBuffer, chunkItemEntriesCount = 1}
                            ]
                          )
                      else do
                        newOffset <- unsafeAppendToBuffer storage 0 entry
                        let digest' = merkleUpdate digest (CStringLenBuffer (mutableByteArrayContents storage `plusPtr` offset, l))
                        pure
                          ( machine 1 newOffset digest'
                          , [ChunkItem{chunkItemFormat = format, chunkItemData = frozenBuffer, chunkItemEntriesCount = entriesCount}]
                          )
          }
  return $! machine 0 0 merkleInitialize

{- | Freeze a bytearray to the pinned immutable bytearray by copying its contents.

It's safe to use the source bytearray after this operation.
-}
freezeByteArrayPinned :: (PrimMonad m) => MutableByteArray (PrimState m) -> Int -> Int -> m ByteArray
freezeByteArrayPinned !src !off !len = do
  dst <- newPinnedByteArray len
  copyMutableByteArray dst 0 src off len
  unsafeFreezeByteArray dst

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
