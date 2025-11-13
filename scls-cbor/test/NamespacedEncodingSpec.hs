{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tests for namespace-versioned encoding and decoding.

This module tests the namespace-versioned serialization mechanism that
associates data types with specific namespaces and versions.
-}
module NamespacedEncodingSpec where

import Cardano.SCLS.CBOR.Canonical.NamespacedEncoder (
  CanonicalDecoder (decodeNamespaced),
  CanonicalEncoder (encodeNamespaced),
  VersionedNS (VersionedNS),
 )
import Cardano.SCLS.Internal.Entry (ChunkEntry (ChunkEntry), GenericCBOREntry (GenericCBOREntry))
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import TestEntry

-- | Test suite
spec :: Spec
spec = do
  describe "CanonicalEncoder" $ do
    it "should encode TestEntry in utxo/v0 namespace" $ do
      val <- genUTxO
      let _ = encodeNamespaced @"utxo/v0" val
      -- Just checking if the compiler and type-checker are fine
      pure ()

    it "should encode TestEntry differently in blocks/v0 namespace" $ do
      val <- genUTxO

      -- Key sizes can be the same, but may not always be the case
      -- Type check fails if key sizes are different and we try to compare them directly as `GenericCBOREntry`
      -- Because of that, we compare the CBOR terms of each encoded value
      let GenericCBOREntry (ChunkEntry _key1 encoded0) = encodeNamespaced @"utxo/v0" val
      let GenericCBOREntry (ChunkEntry _key2 encoded1) = encodeNamespaced @"blocks/v0" val

      encoded0 `shouldNotBe` encoded1

    it "should roundtrip encode/decode TestEntry successfully" $ do
      val <- genUTxO

      let encoded = encodeNamespaced @"utxo/v0" val
      let decoded = decodeNamespaced @"utxo/v0" encoded

      decoded `shouldBe` (Just (VersionedNS val))
