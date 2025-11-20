{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestEntry (
  TestEntry (..),
  TestEntryKey (..),
  NamespacedTestEntry (..),
  toChunkEntry,
  genKey,
  genEntry,
  genUTxO,
  genBlock,
) where

import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (ChunkEntry))
import Cardano.SCLS.Internal.Entry.IsKey (IsKey (keySize, packKeyM, unpackKeyM))
import Cardano.SCLS.Internal.Namespace (CanonicalCBOREntryDecoder (decodeEntry), CanonicalCBOREntryEncoder (encodeEntry), KnownNamespace (NamespaceEntry, NamespaceKey, encodeKey), KnownNamespaceKey, NamespaceKeySize, VersionedNS (VersionedNS))
import Cardano.SCLS.Internal.Serializer.HasKey (HasKey (Key, getKey))
import Cardano.SCLS.Internal.Serializer.MemPack (ByteStringSized (ByteStringSized))
import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.Encoding qualified as E
import Data.ByteString qualified as BS
import Data.Data (Proxy (Proxy))
import Data.MemPack (packByteStringM, unpackByteStringM)
import GHC.TypeLits (KnownNat, natVal)
import System.Random.Stateful (Uniform (uniformM), globalStdGen, uniformByteStringM)

-- | Example data type for testing
newtype TestEntryKey = TestEntryKey BS.ByteString
  deriving (Eq, Ord, Show)

instance IsKey TestEntryKey where
  keySize = 34

  packKeyM (TestEntryKey bs) = packByteStringM bs

  unpackKeyM =
    TestEntryKey <$> unpackByteStringM (keySize @TestEntryKey)

data TestEntry = TestEntry
  { key :: BS.ByteString
  , value :: Int
  }
  deriving (Eq, Show)

instance HasKey TestEntry where
  type Key TestEntry = TestEntryKey

  getKey (TestEntry k _) = TestEntryKey k

toChunkEntry :: TestEntry -> ChunkEntry TestEntryKey TestEntry
toChunkEntry e = ChunkEntry (TestEntryKey $ key e) e

instance CanonicalCBOREntryEncoder "utxo/v0" TestEntry where
  encodeEntry TestEntry{key, value} =
    E.encodeListLen 2 <> E.encodeBytes key <> E.encodeInt value

instance CanonicalCBOREntryDecoder "utxo/v0" TestEntry where
  decodeEntry = do
    D.decodeListLenOf 2
    key <- D.decodeBytes
    value <- D.decodeInt
    pure $ VersionedNS $ TestEntry key value

instance CanonicalCBOREntryEncoder "blocks/v0" TestEntry where
  -- For this test, we reuse the same data type (TestEntry), but we encode it's value as `n+1`.
  encodeEntry TestEntry{key, value} =
    E.encodeListLen 2 <> E.encodeBytes key <> E.encodeInt (value + 1)

instance CanonicalCBOREntryDecoder "blocks/v0" TestEntry where
  decodeEntry = do
    D.decodeListLenOf 2
    key <- D.decodeBytes
    value <- D.decodeInt
    pure $ VersionedNS $ TestEntry key (value - 1)

instance KnownNamespace "utxo/v0" where
  type NamespaceKey "utxo/v0" = TestEntryKey
  type NamespaceEntry "utxo/v0" = TestEntry

  encodeKey (TestEntryKey k) = ByteStringSized k

instance KnownNamespace "blocks/v0" where
  type NamespaceKey "blocks/v0" = TestEntryKey
  type NamespaceEntry "blocks/v0" = TestEntry

  encodeKey (TestEntryKey k) = ByteStringSized k

genKey :: forall ns. (KnownNamespaceKey ns, KnownNat (NamespaceKeySize ns)) => Proxy ns -> IO (ByteStringSized (NamespaceKeySize ns))
genKey _ =
  ByteStringSized <$> uniformByteStringM (fromInteger $ natVal (Proxy :: Proxy (NamespaceKeySize ns))) globalStdGen

newtype NamespacedTestEntry ns = NamespacedTestEntry {unNamespacedTestEntry :: TestEntry}

genEntry :: forall ns. (KnownNamespaceKey ns, KnownNat (NamespaceKeySize ns)) => Proxy ns -> IO (NamespacedTestEntry ns)
genEntry p = do
  (ByteStringSized key) <- genKey p
  value <- uniformM globalStdGen
  pure $ NamespacedTestEntry $ TestEntry key value

genUTxO :: IO (NamespacedTestEntry "utxo/v0")
genUTxO =
  genEntry (Proxy :: Proxy "utxo/v0")

genBlock :: IO (NamespacedTestEntry "blocks/v0")
genBlock =
  genEntry (Proxy :: Proxy "blocks/v0")
