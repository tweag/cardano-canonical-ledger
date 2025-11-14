{-# LANGUAGE DataKinds #-}

module TestEntry (TestEntry, genEntry) where

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

instance CanonicalEncoder "utxo/v0" TestEntry where
  encodeNamespaced TestEntry{key, value} =
    GenericCBOREntry $ ChunkEntry (ByteStringSized key) (CBORTerm $ TInt value)

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

genEntry :: Int -> IO TestEntry
genEntry kSize = do
  key <- uniformByteStringM kSize globalStdGen
  value <- uniformM globalStdGen
  pure $ TestEntry key value

{- | Example usage:

@
  -- To encode
  let x = "utxo/v0"
  let v = TestEntry{key = BS.singleton 1, value = 2}
  let encoded = encodeNamespaced @"utxo/v0" v

  -- To decode
  --
  case x of
    "utxo/v0" -> do
      let decoded = decodeNamespaced @"utxo/v0" @TestEntry encoded
      show decoded
    _ -> do
      error "Unknown namespace"
@
-}
