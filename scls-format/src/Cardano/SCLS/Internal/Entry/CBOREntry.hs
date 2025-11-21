{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.SCLS.Internal.Entry.CBOREntry (
  GenericCBOREntry (..),
  SomeCBOREntry (..),
) where

import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (..))
import Cardano.SCLS.Internal.Serializer.HasKey
import Cardano.SCLS.Internal.Serializer.MemPack (ByteStringSized (..), CBORTerm (..), MemPackHeaderOffset (..), SomeByteStringSized (..))
import Data.MemPack
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownNat, Nat)

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

instance (KnownNat n, Typeable (ByteStringSized n)) => MemPack (GenericCBOREntry n) where
  packedByteCount = packedByteCount . unGenericCBOREntry
  packM = packM . unGenericCBOREntry
  unpackM = GenericCBOREntry <$> unpackM

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
