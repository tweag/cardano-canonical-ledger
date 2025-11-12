{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.SCLS.Internal.Frame (
  FrameView (..),
  mkRecordType,
  fetchOffsetFrame,
  fetchNextFrame,
  frameHeaderSize,

  -- * IO
  headerOffset,
  decodeFrame,
  hWriteFrame,
  hWriteFrameBuffer,
  Debug (..),
) where

-- TODO use something more efficient
-- TODO: use something more efficient (or lower level)

-- TODO: remove, it's for debug

import Cardano.SCLS.Internal.Record.Internal.Class
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS8
import Data.MemPack (StateT (runStateT), Unpack (runUnpack), packWithByteArray, unpackError)
import Data.MemPack.Buffer
import Data.MemPack.ByteOrdered (BigEndian (BigEndian))
import Data.Proxy
import Data.Typeable (Typeable, typeOf)
import Data.Word (Word32, Word64, Word8)

-- TODO: move to IO module?
import Data.MemPack.Error (Error (toSomeError), SomeError, TextError (TextError))
import Data.MemPack.Extra (hPutBuffer, runDecode)
import GHC.TypeLits
import System.IO

{- | A structure for a container in the SCLS format

Frame is a record with the size and its contents in binary
form.

The frame is opaque object so we can use it in order to parse
data in a different form, or bypass parsing entiry.

Contents field is deliberately left lazy.
-}
data FrameView a = FrameView
  { frameViewSize :: Word32
  , frameRecordType :: Word8
  , frameViewContent :: ~a
  }
  deriving (Show)
  deriving (Functor) -- TODO: remove, it's actally illegal

newtype Debug = Debug {unDebug :: BS.ByteString}

instance Show Debug where
  show (Debug bs)
    | 9 > BS.length bs = BS8.unpack $ Base16.encode bs
    | otherwise = BS8.unpack $ Base16.encode (BS.take 4 bs) <> "..." <> Base16.encode (BS.takeEnd 4 bs)

{- | Frame that is just an offset in the file. It's used for
pre-parsing and fast seekings or skipping frames.
-}
type OffsetFrame = FrameView Word64

{- | Frame that keeps a ByteArray as a buffer, data is not yet parsed
though. Usually used for reading or generating a Frame.
-}
type ByteArrayFrame = FrameView BS.ByteString

{- | /O(N)/ Materialize data in the frame by fetching it from the file.

This method is used to decouple the parsing and interpretation of the
record from IO operations

The position of the `Handle` will be updated to the end of the fetched record.
-}
fetchOffsetFrame :: Handle -> OffsetFrame -> IO (ByteArrayFrame)
fetchOffsetFrame handle (FrameView size record_type offset) = do
  hSeek handle AbsoluteSeek (fromIntegral offset)
  contents <- BS.hGet handle (fromIntegral size)
  return $ FrameView size record_type contents

{- | Try to decode a frame from its byte array representation.

In order to use this method, the programmer should know the type to the
record in advance.
-}
decodeFrame :: forall t b. (IsFrameRecord t b) => ByteArrayFrame -> Either SomeError (FrameView b)
decodeFrame (FrameView size record_type contents) = do
  if natVal (Proxy @t) == fromIntegral record_type
    then
      fmap
        (FrameView size record_type . fst)
        $ runDecode
        $ runStateT (runUnpack (decodeRecordContents size) contents) 1
    else Left $ toSomeError $ TextError "Unknown record type"

{- | /O(1)/ Read the next frame from the handle.

Require that Handle is the same one an OffsetFrame was created from.

@throws 'IOException'
- 'isIllegalOperationError' if the Handle is not seekable, or does not support the requested seek mode.
- 'isPermissionError' if a system resource limit would be exceeded.
-}
fetchNextFrame :: Handle -> OffsetFrame -> IO (Maybe OffsetFrame)
fetchNextFrame handle (FrameView size _type offset) = do
  hSeek handle AbsoluteSeek (fromIntegral next_offset)
  bs <- BS.hGet handle 4
  if BS.length bs < 4
    then return Nothing
    else do
      [record_type] <- BS.unpack <$> BS.hGet handle 1 -- TODO: handle eOF
      let BigEndian next_size = unpackError bs
      return $ Just (FrameView next_size record_type (4 + next_offset))
 where
  next_offset = offset + fromIntegral size

-- | Write a frame to the handle.
hWriteFrame :: forall t b. (Typeable b, IsFrameRecord t b) => Handle -> b -> IO Int
hWriteFrame handle b =
  let contents = packWithByteArray True (show $ typeOf b) (frameRecordSize b) (encodeRecordContents b)
      record_type = fromIntegral (natVal (Proxy @t))
   in hWriteFrameBuffer handle record_type contents

{- | Write contents in the frame to the handle in the .

Returns number of bytes written to Handle.
-}
hWriteFrameBuffer :: (Buffer u) => Handle -> Word8 -> u -> IO Int
hWriteFrameBuffer handle record_type u = do
  Builder.hPutBuilder handle (Builder.word32BE (fromIntegral len + 1))
  Builder.hPutBuilder handle (Builder.word8 record_type)
  hPutBuffer handle u
  hFlush handle
  return $! 5 + len
 where
  len = bufferByteCount u

-- | Zero frame that is useful to initialize process of file consumption
headerOffset :: OffsetFrame
headerOffset = FrameView 0 0 0

-- | Size of the frame, offset from the start to the first byte of the payload.
frameHeaderSize :: (Num a) => a
{-# SPECIALIZE frameHeaderSize :: Word32 #-}
frameHeaderSize = 5
