{-# LANGUAGE AllowAmbiguousTypes #-}

module Cardano.SCLS.Internal.Entry (
  IsKey (..),
  ChunkEntry (..),
) where

import Cardano.Types.ByteOrdered (BigEndian (..))
import Data.MemPack
import Data.MemPack.Buffer
import Data.Typeable
import Data.Word (Word32)

class IsKey a where
  keySize :: Int
  packKeyM :: a -> Pack b ()
  unpackKeyM :: (Buffer s) => Unpack s a

data ChunkEntry k v = ChunkEntry
  { chunkEntryKey :: k
  , chunkEntryValue :: v
  }
  deriving (Show)

instance (Typeable k, IsKey k, MemPack v, Typeable v) => MemPack (ChunkEntry k v) where
  packedByteCount (ChunkEntry _ v) = 4 + keySize @k + packedByteCount v
  packM (ChunkEntry k v) = do
    let lenEntry = keySize @k + packedByteCount v
    packM (BigEndian (fromIntegral lenEntry :: Word32))
    packKeyM k
    packM v
  unpackM = do
    BigEndian (_lenEntry :: Word32) <- unpackM
    k <- unpackKeyM
    v <- unpackM
    return (ChunkEntry k v)

instance (Eq k, Eq v) => Eq (ChunkEntry k v) where
  (ChunkEntry k1 v1) == (ChunkEntry k2 v2) = k1 == k2 && v1 == v2

instance (Ord k, Ord v) => Ord (ChunkEntry k v) where
  compare (ChunkEntry k1 v1) (ChunkEntry k2 v2) = compare k1 k2 <> compare v1 v2
