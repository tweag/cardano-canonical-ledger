{-# LANGUAGE DataKinds #-}
module Cardano.SCLS.Internal.Record.Hdr
  ( Hdr(..)
  , mkHdr
  ) where

import Foreign

import Cardano.SCLS.Internal.Record.Internal.Class
import Cardano.SCLS.Internal.Version (Version(..), packVersion)
import Cardano.Types.Network
import Cardano.Types.SlotNo

import System.IO.Unsafe (unsafePerformIO) -- TODO: switch to non-pure interface instead
import Data.Binary.Put (putBuilder)
import Data.Binary.Get.Internal (readN)
import Data.ByteString.Builder.Internal hiding (putBuilder)
import Data.ByteString.Unsafe (unsafeUseAsCString)

-- | Header record.
data Hdr = Hdr
  { magic :: Word64
  , version :: Word32
  , networkId :: NetworkId
  , slotNo :: SlotNo
  }
  deriving (Show)
  -- deriving MemPack via (AsStorable Hdr)

instance IsFrameRecord 0 Hdr where
  encodeRecordContents a = putBuilder (ensureFree (sizeOf (undefined::Hdr)) <> builder step) where
    step k (BufferRange op ope) = do
      poke (castPtr op) a
      k (BufferRange (op `advancePtr` sizeOf (undefined :: Hdr)) ope)
  decodeRecordContents = readN (sizeOf (undefined :: Hdr)) $ \b ->
    unsafePerformIO $ unsafeUseAsCString b (peek . castPtr)

-- | Storable instance for a Header record
instance Storable Hdr where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = do
    magic_pre <- peekByteOff ptr 0
    let magic = magic_pre .&. 0xfffff000
    version <- peekByteOff ptr 8
    networkId <- peekByteOff ptr 12
    slotNo <- peekByteOff ptr 16
    return $! Hdr magic version networkId slotNo
  poke ptr (Hdr magic version networkId slotNo) = do
    pokeByteOff ptr 0 magic
    pokeByteOff ptr 8 version
    pokeByteOff ptr 12 networkId
    pokeByteOff ptr 16 slotNo


-- | Creates header record for the current version.
mkHdr :: NetworkId -> SlotNo -> Hdr
mkHdr networkId slotNo = Hdr
  { magic = 1397506899 -- "SCLS\0"
  , version = packVersion V1
  , networkId = networkId
  , slotNo = slotNo
  }
