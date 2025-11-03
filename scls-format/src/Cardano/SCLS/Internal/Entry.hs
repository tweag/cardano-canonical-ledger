{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.SCLS.Internal.Entry (
  IsKey (..),
  ChunkEntry (..),
  GenericCBOREntry (..),
  SomeCBOREntry (..),
) where

import Cardano.SCLS.Internal.Serializer.HasKey
import Cardano.SCLS.Internal.Serializer.MemPack (ByteStringSized (..), CBORTerm (..), MemPackHeaderOffset (..), SomeByteStringSized (..), isolated)
import Cardano.Types.ByteOrdered (BigEndian (..))
import Data.MemPack
import Data.MemPack.Buffer
import Data.Typeable
import Data.Word (Word32)
import GHC.TypeLits (KnownNat, Nat, natVal)

class (Ord a) => IsKey a where
  keySize :: Int
  packKeyM :: a -> Pack b ()
  unpackKeyM :: (Buffer s) => Unpack s a

data ChunkEntry k v = ChunkEntry
  { chunkEntryKey :: k
  , chunkEntryValue :: v
  }
  deriving (Show)

instance (Ord k) => HasKey (ChunkEntry k v) where
  type Key (ChunkEntry k v) = k
  getKey (ChunkEntry k _) = k

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

instance (Typeable k, IsKey k, Typeable v, MemPack v) => MemPackHeaderOffset (ChunkEntry k v) where
  headerSizeOffset = 4

instance (Eq k, Eq v) => Eq (ChunkEntry k v) where
  (ChunkEntry k1 v1) == (ChunkEntry k2 v2) = k1 == k2 && v1 == v2

instance (Ord k, Ord v) => Ord (ChunkEntry k v) where
  compare (ChunkEntry k1 v1) (ChunkEntry k2 v2) = compare k1 k2 <> compare v1 v2

{- | A generic chunk entry that we are not aware of exact content type,
except that it is a valid CBOR, as required per SCLS specification.
-}
newtype GenericCBOREntry (n :: Nat) = GenericCBOREntry
  { unGenericCBOREntry :: ChunkEntry (ByteStringSized n) CBORTerm
  }
  deriving (Show, Eq, Ord)

instance HasKey (GenericCBOREntry n) where
  type Key (GenericCBOREntry n) = ByteStringSized n
  getKey (GenericCBOREntry (ChunkEntry k _)) = k

instance (KnownNat n) => IsKey (ByteStringSized n) where
  keySize = fromInteger (natVal (Proxy :: Proxy n))
  packKeyM = packM
  unpackKeyM = unpackM

instance (KnownNat n, Typeable (ByteStringSized n)) => MemPack (GenericCBOREntry n) where
  packedByteCount (GenericCBOREntry ce) = packedByteCount ce
  packM (GenericCBOREntry ce) = packM ce
  unpackM =
    GenericCBOREntry <$> do
      BigEndian (lenEntry :: Word32) <- unpackM
      key <- unpackKeyM
      value <- isolated (fromIntegral lenEntry - fromIntegral (keySize @(ByteStringSized n)))
      return (ChunkEntry key value)

instance (KnownNat n) => MemPackHeaderOffset (GenericCBOREntry n) where
  headerSizeOffset = 4

{- | An existential wrapper that allows to store 'GenericCBOREntry' of any
size in the same plan.
-}
data SomeCBOREntry = forall n. (KnownNat n) => SomeCBOREntry (GenericCBOREntry n)

instance MemPack SomeCBOREntry where
  packedByteCount (SomeCBOREntry gce) = packedByteCount gce
  packM (SomeCBOREntry gce) = packM gce
  unpackM = error "unpackM SomeCBOREntry: cannot determine size n at runtime"

instance MemPackHeaderOffset SomeCBOREntry where
  headerSizeOffset = 4

instance HasKey SomeCBOREntry where
  type Key SomeCBOREntry = SomeByteStringSized
  getKey (SomeCBOREntry gce) = SomeByteStringSized (getKey gce)
