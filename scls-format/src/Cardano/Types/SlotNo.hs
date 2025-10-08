{-# LANGUAGE DerivingVia #-}

-- | Slot number.
module Cardano.Types.SlotNo (SlotNo (..)) where

import Cardano.Types.ByteOrdered
import Data.MemPack
import Foreign

-- TODO: define mempack or another class type for data serialisation

newtype SlotNo = SlotNo {unSlotNo :: Word64}
  deriving (Ord, Eq, Show, Read)
  deriving (Storable) via (BigEndian Word64)
  deriving (MemPack) via (BigEndian Word64)
