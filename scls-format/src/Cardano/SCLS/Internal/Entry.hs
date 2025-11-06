{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.SCLS.Internal.Entry (
  IsKey (..),
  ChunkEntry (..),
  GenericCBOREntry (..),
  SomeCBOREntry (..),
  sortByKey,
) where

import Cardano.SCLS.Internal.Serializer.HasKey
import Cardano.SCLS.Internal.Serializer.MemPack (ByteStringSized (..), CBORTerm (..), MemPackHeaderOffset (..), SomeByteStringSized (..))
import Data.List (sortOn)
import Data.MemPack
import Data.MemPack.Buffer
import Data.Typeable
import GHC.TypeLits (KnownNat, Nat, natVal)

class (Ord a) => IsKey a where
  keySize :: Int
  packKeyM :: a -> Pack b ()
  unpackKeyM :: forall b s. (Buffer b) => Unpack s b a

data ChunkEntry k v = ChunkEntry
  { chunkEntryKey :: k
  , chunkEntryValue :: v
  }
  deriving (Show)

instance (Ord k) => HasKey (ChunkEntry k v) where
  type Key (ChunkEntry k v) = k
  getKey (ChunkEntry k _) = k

instance (Typeable k, IsKey k, MemPack v, Typeable v) => MemPack (ChunkEntry k v) where
  packedByteCount (ChunkEntry _ v) = keySize @k + packedByteCount v
  packM (ChunkEntry k v) = do
    packKeyM k
    packM v
  unpackM = do
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
      key <- unpackKeyM
      value <- unpackM
      return (ChunkEntry key value)

instance (KnownNat n) => MemPackHeaderOffset (GenericCBOREntry n) where
  headerSizeOffset = headerSizeOffset @(ChunkEntry (ByteStringSized n) CBORTerm)

{- | An existential wrapper that allows to store 'GenericCBOREntry' of any
size in the same plan.
-}
data SomeCBOREntry = forall n. (KnownNat n) => SomeCBOREntry (GenericCBOREntry n)

instance Show SomeCBOREntry where
  show (SomeCBOREntry gce) = show gce

instance Eq SomeCBOREntry where
  (SomeCBOREntry (GenericCBOREntry (ChunkEntry key1 v1))) == (SomeCBOREntry (GenericCBOREntry (ChunkEntry key2 v2))) =
    (SomeByteStringSized key1) == (SomeByteStringSized key2) && v1 == v2

instance MemPack SomeCBOREntry where
  packedByteCount (SomeCBOREntry gce) = packedByteCount gce
  packM (SomeCBOREntry gce) = packM gce
  unpackM = error "unpackM SomeCBOREntry: cannot determine size n at runtime"

instance MemPackHeaderOffset SomeCBOREntry where
  headerSizeOffset = headerSizeOffset @(GenericCBOREntry 1) -- size does not matter here

instance HasKey SomeCBOREntry where
  type Key SomeCBOREntry = SomeByteStringSized
  getKey (SomeCBOREntry gce) = SomeByteStringSized (getKey gce)

sortByKey :: [SomeCBOREntry] -> [SomeCBOREntry]
sortByKey = sortOn getKey
