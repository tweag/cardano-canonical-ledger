{-# LANGUAGE DerivingVia #-}

module Cardano.Types.SlotNo (SlotNo (..)) where

import Foreign

-- TODO: define mempack or another class type for data serialisation

newtype SlotNo = SlotNo {unSlotNo :: Word64}
  deriving (Eq, Ord, Show, Read)
  deriving (Storable) via Word64
