{-# LANGUAGE DataKinds #-}

module Cardano.SCLS.Internal.Record.Hdr (
  Hdr (..),
  mkHdr,
) where

import Foreign

import Cardano.SCLS.Internal.Record.Internal.Class
import Cardano.SCLS.Internal.Version (Version (..), packVersion, unpackVersion)
import Cardano.Types.Network
import Cardano.Types.SlotNo

-- TODO: switch to non-pure interface instead

import Data.Binary.Get.Internal (readN)
import Data.Binary.Put (putBuilder)
import Data.ByteString.Builder.Internal hiding (putBuilder)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import System.IO.Unsafe (unsafePerformIO)

-- | Header record.
data Hdr = Hdr
  { magic :: Word64
  , version :: Version
  , networkId :: NetworkId
  , slotNo :: SlotNo
  }
  deriving (Show)

-- deriving MemPack via (AsStorable Hdr)

instance IsFrameRecord 0 Hdr where
  encodeRecordContents a = putBuilder (ensureFree (sizeOf (undefined :: Hdr)) <> builder step)
   where
    step k (BufferRange op ope) = do
      poke (castPtr op) a
      k (BufferRange (op `advancePtr` sizeOf (undefined :: Hdr)) ope)
  decodeRecordContents = readN (sizeOf (undefined :: Hdr)) $ \b ->
    unsafePerformIO $ unsafeUseAsCString b (peek . castPtr)

-- | Storable instance for a Header record
instance Storable Hdr where
  sizeOf _ =
    (sizeOf (undefined :: Word64))
      + 4
      + (sizeOf (undefined :: Word32))
      + (sizeOf (undefined :: NetworkId))
      + (sizeOf (undefined :: SlotNo))
  alignment _ = 8
  peek ptr = do
    magic_pre <- peekByteOff ptr 0
    let magic = magic_pre .&. 0xffff0000 -- We are interested only in the first 4 bytes
    version <- unpackVersion <$> peekByteOff ptr 4
    networkId <- peekByteOff ptr 8
    slotNo <- peekByteOff ptr 9
    return $! Hdr magic version networkId slotNo
  poke ptr (Hdr magic version networkId slotNo) = do
    pokeByteOff ptr 0 magic
    pokeByteOff ptr 4 (packVersion version)
    pokeByteOff ptr 8 networkId
    pokeByteOff ptr 9 slotNo

-- | Creates header record for the current version.
mkHdr :: NetworkId -> SlotNo -> Hdr
mkHdr networkId slotNo =
  Hdr
    { magic = 1397506899 -- "SCLS"
    , version = V1
    , networkId = networkId
    , slotNo = slotNo
    }
