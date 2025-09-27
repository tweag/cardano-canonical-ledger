-- | Helpers for dealing with endianness in binary formats.
module Cardano.Types.ByteOrdered (
  BigEndian (..),
  MemPack (..),
  Storable (..),
) where

import Data.MemPack (MemPack (..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import System.ByteOrder (Bytes (..), fromBigEndian, toBigEndian)

{- | A wrapper type to indicate that a value is stored in big-endian format.

Intended to be used with 'DerivingVia' to derive instances.

Example:
@
newtype SlotNo = SlotNo { unSlotNo :: Word64 }
  deriving (Eq, Ord, Show, Read)
  deriving (Storable) via (BigEndian Word64)
@
-}
newtype BigEndian a = BigEndian {unBigEndian :: a}
  deriving (Eq, Ord, Show, Read)

instance (Bytes a, Storable a) => Storable (BigEndian a) where
  sizeOf _ = sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    w <- peek (castPtr ptr :: Ptr a)
    return (BigEndian (fromBigEndian w))
  poke ptr (BigEndian w) = poke (castPtr ptr :: Ptr a) (toBigEndian w)

instance (Bytes a, MemPack a) => MemPack (BigEndian a) where
  typeName = "BigEndian " ++ typeName @a
  packedByteCount (BigEndian a) = packedByteCount a
  packM (BigEndian a) = packM (toBigEndian a)
  unpackM = BigEndian . fromBigEndian <$> unpackM
