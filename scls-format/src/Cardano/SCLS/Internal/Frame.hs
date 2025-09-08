{-# LANGUAGE OverloadedStrings #-}
module Cardano.SCLS.Internal.Frame
  ( FrameView(..)
  , fetchOffsetFrame
  , fetchNextFrame
    -- * IO
  , headerOffset
  , decodeFrame
  , hWriteFrame
  , hWriteFrameBuffer
  , Debug(..)
  ) where

import Data.Binary.Get (runGet, getWord32be) -- TODO use something more efficient
import Data.Binary.Put (runPut) -- TODO: use something more efficient (or lower level)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8 -- TODO: remove, it's for debug
import Data.Proxy
import Data.MemPack.Buffer
import Data.Word (Word64, Word32, Word8)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Base16 qualified as Base16
import GHC.ForeignPtr
import Cardano.SCLS.Internal.Record.Internal.Class
import GHC.Ptr
import System.IO -- TODO: move to IO module?
import GHC.TypeLits

-- | A structure for a container in the SCLS format
--
-- Frame is a record with the size and its contents in binary
-- form.
--
-- The frame is opaque object so we can use it in order to parse
-- data in a different form, or bypass parsing entiry.
--
-- Contents field is deliberately left lazy.
data FrameView a = FrameView
  { frameViewSize :: Word32
  , frameRecordType :: Word8
  , frameViewContent :: ~a
  }
  deriving Show
  deriving (Functor) -- TODO: remove, it's actally illegal

newtype Debug = Debug { unDebug :: BS.ByteString }

instance Show Debug where
    show (Debug bs)
      | 9 > BS.length bs = BS8.unpack $ Base16.encode bs
      | otherwise = BS8.unpack $ Base16.encode (BS.take 4 bs) <> "..." <> Base16.encode (BS.takeEnd 4 bs)

-- | Frame that is just an offset in the file. It's used for
-- pre-parsing and fast seekings or skipping frames.
type OffsetFrame = FrameView Word64

-- | Frame that keeps a ByteArray as a buffer, data is not yet parsed
-- though. Usually used for reading or generating a Frame.
type ByteArrayFrame = FrameView BS.ByteString

-- | /O(N)/ Materialize data in the frame by fetching it from the file.
--
-- This method is used to decouple the parsing and interpretation of the
-- record from IO operations
--
-- The position of the `Handle` will be updated to the end of the fetched record.
fetchOffsetFrame :: Handle -> OffsetFrame -> IO (ByteArrayFrame)
fetchOffsetFrame handle (FrameView size record_type offset) = do
    hSeek handle AbsoluteSeek (fromIntegral offset)
    contents <- BS.hGet handle (fromIntegral size)
    return $ FrameView size record_type contents

-- | Try to decode a frame from its byte array representation.
--
-- In order to use this method, the programmer should know the type to the
-- record in advance.
decodeFrame :: forall t b . IsFrameRecord t b => ByteArrayFrame -> Maybe (FrameView b)
decodeFrame (FrameView size record_type contents) = do
    if natVal (Proxy :: Proxy t) ==  fromIntegral record_type
         -- TODO this thing may fail, we need to be more careful here
    then let decoded_record = runGet decodeRecordContents (BS.fromStrict contents)
         in Just (FrameView size record_type decoded_record)
    else Nothing

-- | /O(1)/ Read the next frame from the handle.
--
-- Require that Handle is the same one an OffsetFrame was created from.
--
-- @throws 'IOException'
-- - 'isIllegalOperationError' if the Handle is not seekable, or does not support the requested seek mode.
-- - 'isPermissionError' if a system resource limit would be exceeded.
fetchNextFrame :: Handle -> OffsetFrame -> IO (Maybe OffsetFrame)
fetchNextFrame handle (FrameView size _type offset) = do
    hSeek handle AbsoluteSeek (fromIntegral next_offset)
    bs <- BS.hGet handle 4
    if BS.length bs < 4
    then return Nothing
    else do
      [record_type] <- BS.unpack <$> BS.hGet handle 1 -- TODO: handle eOF
      let next_size = runGet getWord32be (BS.fromStrict bs) -- Write class for BE for mempack and use it!
      return $ Just (FrameView next_size record_type (4 + next_offset))
  where
    next_offset = offset + fromIntegral size

-- | Write a frame to the handle.
hWriteFrame :: forall t b . IsFrameRecord t b => Handle -> b -> IO Int
hWriteFrame handle b =
    let contents = runPut (encodeRecordContents b)
        record_type = fromIntegral (natVal (Proxy :: Proxy t))
    in hWriteFrameBuffer handle record_type (BS.toStrict contents)

-- | Write contents in the frame to the handle in the .
--
-- Returns number of bytes written to Handle.
hWriteFrameBuffer :: Buffer u => Handle -> Word8 -> u -> IO Int
hWriteFrameBuffer handle record_type u = do
    Builder.hPutBuilder handle (Builder.word32BE (fromIntegral len+1))
    Builder.hPutBuilder handle (Builder.word8 record_type)
    buffer u
      (\bytes -> do
        withForeignPtr (pinnedByteArrayToForeignPtr bytes) $ \ptr -> do
            hPutBuf handle ptr len)
      (\addr -> hPutBuf handle (Ptr addr) len) -- Write Ptr#
    hFlush handle
    return $! 5 + len
  where
    len = bufferByteCount u

-- | Zero frame that is useful to initialize process of file consumption
headerOffset :: OffsetFrame
headerOffset = FrameView 0 0 0
