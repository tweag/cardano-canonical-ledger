{-# LANGUAGE DerivingVia #-}

-- | Slot number.
module Cardano.SCLS.Epoch (Epoch (..)) where

import Data.MemPack
import Data.MemPack.ByteOrdered
import Foreign

newtype Epoch = Epoch {unEpoch :: Word64}
  deriving (Ord, Eq, Show, Read)
  deriving (Storable) via (BigEndian Word64)
  deriving (MemPack) via (BigEndian Word64)
