{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Useful utilities for working with MemPack types.
module Cardano.SCLS.Internal.Serializer.MemPack (
  Entry (..),
  RawBytes (..),
  CStringLenBuffer (..),
  isolate,
) where

import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Fail
import Data.ByteArray (ByteArrayAccess, length, withByteArray)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.MemPack
import Data.MemPack.Buffer
import Data.MemPack.Error
import Data.Primitive.Ptr
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import GHC.Stack (HasCallStack)
import System.ByteOrder

{- | Wrapper that allows to store raw bytes without any prefix.

It's likely that this type will be removed in the future as
it does not provide a way to decode the data back.
-}
newtype RawBytes = RawBytes ByteString
  deriving (Eq, Ord, Show)

-- Instance that reads all remaining bytes as a ByteString, relies
-- on running in 'isolated' context.
instance MemPack (RawBytes) where
  packedByteCount (RawBytes bs) = BS.length bs
  packM (RawBytes bs) = packByteStringM bs
  unpackM = do
    len <- Unpack $ \b ->
      get >>= \s -> do
        return (bufferByteCount b - s)
    RawBytes <$> unpackByteStringM len

{- | Entry wrapper for other mempack values, that explicitly
stores its length as a big-endian 'Word32' before the value
itself
-}
newtype Entry u = Entry u
  deriving (Eq, Ord)

instance (Typeable u, MemPack u) => MemPack (Entry u) where
  packedByteCount (Entry u) = 4 + packedByteCount u
  packM (Entry u) = do
    let l = packedByteCount u
    packM (toBigEndian (fromIntegral l :: Word32)) -- length prefix
    packM u
  unpackM = do
    l :: Word32 <- fromBigEndian <$> unpackM
    u <- isolated (fromIntegral l)
    return (Entry u)

{- | Isolate a decoder to operate with a fixed number of bytes, and fail if fewer bytes
were consumed, or more bytes were attempted to be consumed. If the given decoder fails,
isolate will also fail.
-}
isolated :: forall a b. (Buffer b, MemPack a) => (HasCallStack) => Int -> Unpack b a
isolated len = do
  start <- get
  b' :: Isolate b <- asks (isolate start len)
  a <- Unpack $ \_ -> StateT (\_ -> unpackLeftOverOff b' start unpackM)
  return a

unpackLeftOverOff :: forall a b. (MemPack a, Buffer b) => (HasCallStack) => b -> Int -> Unpack b a -> Fail SomeError (a, Int)
unpackLeftOverOff buf off action = do
  let len = bufferByteCount buf
  res@(_, consumedBytes) <- runStateT (runUnpack action buf) off
  case len `compare` consumedBytes of
    EQ -> return res
    GT ->
      failT $
        toSomeError
          NotFullyConsumedError
            { notFullyConsumedRead = consumedBytes - off
            , notFullyConsumedAvailable = len - consumedBytes
            , notFullyConsumedTypeName = typeName @a
            }
    LT ->
      failT $
        toSomeError
          RanOutOfBytesError
            { ranOutOfBytesRead = consumedBytes - off
            , ranOutOfBytesRequested = len - consumedBytes - off
            , ranOutOfBytesAvailable = 0
            }

{- | Isolate a portion of a buffer to a given offset and length.

As the buffers are not sliceable it is done by keeping information
about the size of the new buffer.
-}
isolate :: (Buffer u) => Int -> Int -> u -> Isolate u
isolate off len buf = Isolate buf (min (bufferByteCount buf) (off + len))

data Isolate b = Isolate
  { isolatedBuffer :: b
  , isolatedLength :: Int
  }
  deriving (Show)

instance (Buffer b) => Buffer (Isolate b) where
  bufferByteCount Isolate{isolatedLength = len} = len
  buffer Isolate{isolatedBuffer = b} f g = buffer b f g

-- | Additional wrapper to use MemPack Buffer interface
newtype CStringLenBuffer = CStringLenBuffer CStringLen

instance Buffer CStringLenBuffer where
  bufferByteCount (CStringLenBuffer (_, l)) = l
  buffer (CStringLenBuffer (ptr, off)) _ withAddr =
    withAddr (case ptr `plusPtr` off of Ptr addr -> addr)

instance ByteArrayAccess CStringLenBuffer where
  length (CStringLenBuffer (_, l)) = l
  withByteArray (CStringLenBuffer (ptr, _)) f =
    f (ptr `plusPtr` 0)
