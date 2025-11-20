{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.SCLS.Internal.Entry.ChunkEntry (
  ChunkEntry (..),
  KnownNamespace (..),
  NamespaceKeySize,
  SomeChunkEntry (..),
  NamespaceChunkEntry,
  unpackSomeChunkEntry,
) where

import Cardano.SCLS.Internal.Entry (IsKey (..))
import Cardano.SCLS.Internal.Namespace (KnownNamespace (..), NamespaceKeySize)
import Cardano.SCLS.Internal.Serializer.HasKey
import Cardano.SCLS.Internal.Serializer.MemPack (ByteStringSized (..), MemPackHeaderOffset (..))
import Data.ByteString qualified as BS
import Data.MemPack
import Data.MemPack.Buffer
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownNat, KnownSymbol)

data ChunkEntry k v = ChunkEntry
  { chunkEntryKey :: k
  , chunkEntryValue :: v
  }
  deriving (Show)

instance (Eq k, Eq v) => Eq (ChunkEntry k v) where
  (ChunkEntry k1 v1) == (ChunkEntry k2 v2) = k1 == k2 && v1 == v2

instance (Ord k, Ord v) => Ord (ChunkEntry k v) where
  compare (ChunkEntry k1 v1) (ChunkEntry k2 v2) = compare k1 k2 <> compare v1 v2

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

type NamespaceChunkEntry ns = ChunkEntry (NamespaceKey ns) (NamespaceEntry ns)

data SomeChunkEntry a = forall n. (KnownNat n) => SomeChunkEntry (ChunkEntry (ByteStringSized n) a)

instance (Show a) => Show (SomeChunkEntry a) where
  show (SomeChunkEntry e) = "SomeChunkEntry " ++ show e

instance (Eq a) => Eq (SomeChunkEntry a) where
  (==) (SomeChunkEntry (ChunkEntry (ByteStringSized k1) v1)) (SomeChunkEntry (ChunkEntry (ByteStringSized k2) v2)) =
    if k1 == k2
      then
        v1 == v2
      else False

instance (Ord a) => Ord (SomeChunkEntry a) where
  compare
    (SomeChunkEntry (ChunkEntry (ByteStringSized k1) v1))
    (SomeChunkEntry (ChunkEntry (ByteStringSized k2) v2)) =
      if k1 == k2
        then
          compare v1 v2
        else compare k1 k2

instance (MemPack a, Typeable a) => MemPack (SomeChunkEntry a) where
  packedByteCount (SomeChunkEntry e) = packedByteCount e

  packM :: (MemPack a, Typeable a) => SomeChunkEntry a -> Pack s ()
  packM (SomeChunkEntry e) = packM e

  unpackM = error "unpackM SomeChunkEntry: cannot determine size at runtime"

unpackSomeChunkEntry ::
  ( KnownSymbol ns
  , KnownNamespace ns
  , MemPack a
  , Typeable a
  , Buffer b
  ) =>
  Unpack s b (ChunkEntry (ByteStringSized (NamespaceKeySize ns)) a)
unpackSomeChunkEntry = do
  e <- unpackM
  pure e

-- instance (KnownNamespace ns) => MemPack (ChunkEntry ns) where
--   typeName :: (KnownNamespace ns) => String
--   typeName = "GenericEntry"

--   packedByteCount ChunkEntry{..} =
--     keySize @(NamespaceKey ns)
--       + (BS.length $ toStrictByteString $ encodeEntry @ns chunkEntryValue)

--   packM ChunkEntry{..} = do
--     packKeyM chunkEntryKey
--     packNamespaceEntryM @ns chunkEntryValue

--   unpackM = do
--     chunkEntryKey <- unpackKeyM
--     valueBytes <- unpackM
--     case deserialiseFromBytes (decodeEntry @ns) valueBytes of
--       Right (_, VersionedNS chunkEntryValue) ->
--         pure $ ChunkEntry{..}
--       Left _err -> fail "CBOR decoding error"

-- instance (Typeable k, IsKey k, MemPack v, Typeable v) => MemPack (ChunkEntry k v) where
--   packedByteCount (ChunkEntry _ v) = keySize @k + packedByteCount v
--   packM (ChunkEntry k v) = do
--     packKeyM k
--     packM v
--   unpackM = do
--     k <- unpackKeyM
--     v <- unpackM
--     return (ChunkEntry k v)

instance (MemPack a, Typeable a) => MemPackHeaderOffset (SomeChunkEntry a) where
  headerSizeOffset = 4

instance HasKey (SomeChunkEntry a) where
  type Key (SomeChunkEntry a) = BS.ByteString

  getKey (SomeChunkEntry (ChunkEntry (ByteStringSized bs) _)) = bs
