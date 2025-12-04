{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Internal.Record.Hdr (
  Hdr (..),
  mkHdr,
) where

import Control.Monad.State (MonadState (put))
import Data.MemPack (MemPack (..))
import Foreign

import Cardano.SCLS.Epoch
import Cardano.SCLS.Internal.Record.Internal.Class
import Cardano.SCLS.Internal.Version (Version (..), packVersion, unpackVersion)
import Cardano.Types.Network

-- TODO: switch to non-pure interface instead

-- | Header record.
data Hdr = Hdr
  { magic :: Word64
  , version :: Version
  , networkId :: NetworkId
  , slotNo :: Epoch
  }
  deriving (Show, Eq)

-- deriving MemPack via (AsStorable Hdr)

instance IsFrameRecord 0 Hdr where
  frameRecordSize Hdr{..} =
    4
      + packedByteCount version
      + packedByteCount networkId
      + packedByteCount slotNo

  encodeRecordContents Hdr{..} = do
    packM magic
    put 4
    packM version
    packM networkId
    packM slotNo

  decodeRecordContents _size = do
    magic_pre :: Word64 <- unpackM
    let magic = magic_pre .&. 0xffffffff -- We are interested only in the first 4 bytes
    put 5
    version <- unpackM
    networkId <- unpackM
    slotNo <- unpackM
    pure Hdr{..}

-- | Storable instance for a Header record
instance Storable Hdr where
  sizeOf _ =
    -- (sizeOf (undefined :: Word64))
    4
      + (sizeOf (undefined :: Word32))
      + (sizeOf (undefined :: NetworkId))
      + (sizeOf (undefined :: Epoch))
  alignment _ = 8
  peek ptr = do
    magic_pre <- peekByteOff ptr 0
    let magic = magic_pre .&. 0xffffffff -- We are interested only in the first 4 bytes
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
mkHdr :: NetworkId -> Epoch -> Hdr
mkHdr networkId slotNo =
  Hdr
    { magic = 1397506899 -- "SCLS"
    , version = V1
    , networkId = networkId
    , slotNo = slotNo
    }
