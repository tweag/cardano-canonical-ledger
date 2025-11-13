{-# LANGUAGE DataKinds #-}

module TestEntry (TestEntry, genUTxO) where

import Cardano.SCLS.CBOR.Canonical.NamespacedEncoder (CanonicalDecoder (..), CanonicalEncoder (..), VersionedNS (VersionedNS))
import Cardano.SCLS.Internal.Entry (ChunkEntry (ChunkEntry), GenericCBOREntry (GenericCBOREntry))
import Cardano.SCLS.Internal.Serializer.MemPack (ByteStringSized (ByteStringSized), CBORTerm (CBORTerm))
import Codec.CBOR.Term (Term (TInt))
import Data.ByteString qualified as BS
import System.Random.Stateful (Uniform (uniformM), globalStdGen, uniformByteStringM)

-- | Example data type for testing
data TestEntry = TestEntry
  { key :: BS.ByteString
  , value :: Int
  }
  deriving (Eq, Show)

-- | Instance for utxo/v0 namespace
instance CanonicalEncoder "utxo/v0" TestEntry where
  encodeNamespaced TestEntry{key, value} =
    GenericCBOREntry $ ChunkEntry (ByteStringSized key) (CBORTerm $ TInt value)

-- | Instance for utxo/v0 namespace
instance CanonicalEncoder "blocks/v0" TestEntry where
  encodeNamespaced TestEntry{key, value} =
    -- For this test, we reuse the same data type (TestEntry), but we encode it's value as `n+1`.
    GenericCBOREntry $ ChunkEntry (ByteStringSized key) (CBORTerm $ TInt (value + 1))

instance CanonicalDecoder "utxo/v0" TestEntry where
  decodeNamespaced (GenericCBOREntry (ChunkEntry (ByteStringSized key) (CBORTerm (TInt n)))) =
    Just $ VersionedNS $ (TestEntry key n)
  decodeNamespaced _ = Nothing

instance CanonicalDecoder "blocks/v0" TestEntry where
  decodeNamespaced (GenericCBOREntry (ChunkEntry (ByteStringSized key) (CBORTerm (TInt n)))) =
    Just $ VersionedNS $ (TestEntry key (n - 1))
  decodeNamespaced _ = Nothing

genUTxO :: IO TestEntry
genUTxO = do
  -- Generate a 34-byte key, matching the NamespaceKeySize for "utxo/v0" and "utxo/v1"
  key <- uniformByteStringM 34 globalStdGen
  value <- uniformM globalStdGen
  pure $ TestEntry key value
