{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestEntry (
  TestEntry (..),
  TestEntryKey (..),
  NamespacedTestEntry (..),
  genKey,
  genEntry,
  genUTxO,
  genBlock,
  chunkEntryFromBlock,
  chunkEntryFromUTxO,
) where

import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (ChunkEntry))
import Cardano.SCLS.Internal.Entry.IsKey (IsKey (keySize, packKeyM, unpackKeyM))
import Cardano.SCLS.Internal.NamespaceCodec (CanonicalCBOREntryDecoder (decodeEntry), CanonicalCBOREntryEncoder (encodeEntry), KnownNamespace (NamespaceEntry, NamespaceKey), NamespaceKeySize, VersionedNS (VersionedNS), namespaceKeySize)
import Cardano.SCLS.Internal.Serializer.HasKey (HasKey (Key, getKey))
import Cardano.SCLS.Internal.Serializer.MemPack (ByteStringSized (ByteStringSized))
import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.Encoding qualified as E
import Data.ByteString qualified as BS
import Data.Data (Proxy (Proxy))
import Data.MemPack (MemPack (packM, unpackM), packByteStringM, unpackByteStringM)
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

newtype TestUTxO = TestUTxO TestEntry
  deriving (Eq, Show)

newtype TestUTxOKey = TestUTxOKey BS.ByteString
  deriving (Eq, Ord)

type instance NamespaceKeySize "utxo/v0" = 34
type instance NamespaceKeySize "blocks/v0" = 32

instance IsKey TestUTxOKey where
  keySize = namespaceKeySize @"utxo/v0"

  packKeyM (TestUTxOKey bs) = packM bs

  unpackKeyM = do
    bs <- unpackM
    pure $ TestUTxOKey bs

newtype TestBlock = TestBlock TestEntry
  deriving (Eq, Show)

newtype TestBlockKey = TestBlockKey BS.ByteString
  deriving (Eq, Ord)

instance IsKey TestBlockKey where
  keySize = namespaceKeySize @"blocks/v0"

  packKeyM (TestBlockKey bs) = packM bs

  unpackKeyM = do
    bs <- unpackM
    pure $ TestBlockKey bs

instance HasKey TestUTxO where
  type Key TestUTxO = TestUTxOKey

  getKey (TestUTxO (TestEntry k _)) = TestUTxOKey k

instance HasKey TestBlock where
  type Key TestBlock = TestBlockKey

  getKey (TestBlock (TestEntry k _)) = TestBlockKey k

instance CanonicalCBOREntryEncoder "utxo/v0" TestUTxO where
  encodeEntry (TestUTxO TestEntry{key, value}) =
    E.encodeListLen 2 <> E.encodeBytes key <> E.encodeInt value

instance CanonicalCBOREntryDecoder "utxo/v0" TestUTxO where
  decodeEntry = do
    D.decodeListLenOf 2
    key <- D.decodeBytes
    value <- D.decodeInt
    pure $ VersionedNS $ TestUTxO $ TestEntry key value

instance CanonicalCBOREntryEncoder "blocks/v0" TestBlock where
  -- For this test, we reuse the same data type (TestEntry), but we encode its value as `n+1`.
  encodeEntry (TestBlock TestEntry{key, value}) =
    E.encodeListLen 2 <> E.encodeBytes key <> E.encodeInt (value + 1)

instance CanonicalCBOREntryDecoder "blocks/v0" TestBlock where
  decodeEntry = do
    D.decodeListLenOf 2
    key <- D.decodeBytes
    value <- D.decodeInt
    pure $ VersionedNS $ TestBlock $ TestEntry key (value - 1)

instance KnownNamespace "utxo/v0" where
  type NamespaceKey "utxo/v0" = TestUTxOKey
  type NamespaceEntry "utxo/v0" = TestUTxO

instance KnownNamespace "blocks/v0" where
  type NamespaceKey "blocks/v0" = TestBlockKey
  type NamespaceEntry "blocks/v0" = TestBlock

genKey :: forall ns. (KnownNamespace ns) => Proxy ns -> IO (ByteStringSized (NamespaceKeySize ns))
genKey _ =
  ByteStringSized <$> uniformByteStringM (namespaceKeySize @ns) globalStdGen

newtype NamespacedTestEntry ns = NamespacedTestEntry {unNamespacedTestEntry :: TestEntry}

genEntry :: forall ns. (KnownNamespace ns) => Proxy ns -> IO (NamespacedTestEntry ns)
genEntry p = do
  (ByteStringSized key) <- genKey p
  value <- uniformM globalStdGen
  pure $ NamespacedTestEntry $ TestEntry key value

genUTxO :: IO TestUTxO
genUTxO =
  TestUTxO . unNamespacedTestEntry <$> genEntry (Proxy @"utxo/v0")

genBlock :: IO TestBlock
genBlock =
  TestBlock . unNamespacedTestEntry <$> genEntry (Proxy @"blocks/v0")

chunkEntryFromUTxO :: TestUTxO -> ChunkEntry TestUTxOKey TestUTxO
chunkEntryFromUTxO (e@(TestUTxO (TestEntry k _))) =
  ChunkEntry (TestUTxOKey k) e

chunkEntryFromBlock :: TestBlock -> ChunkEntry TestBlockKey TestBlock
chunkEntryFromBlock (e@(TestBlock (TestEntry k _))) =
  ChunkEntry (TestBlockKey k) e
