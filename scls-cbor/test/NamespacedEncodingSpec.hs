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
  KnownNamespaceKeySize (namespaceKeySize),
  VersionedNS (VersionedNS),
 )
import Cardano.SCLS.Internal.Entry (ChunkEntry (ChunkEntry), GenericCBOREntry (GenericCBOREntry))
import GHC.TypeLits (fromSNat)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import TestEntry

genUTxO :: IO TestEntry
genUTxO = genEntry (fromInteger $ fromSNat (namespaceKeySize @"utxo/v0"))

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

      -- Key sizes here are different, so the type-check would reject comparing the resulting `GenericCBOREntry` values
      -- Because of that, we compare the CBOR terms of each encoded value
      let GenericCBOREntry (ChunkEntry _key1 encoded0) = encodeNamespaced @"utxo/v0" val
      let GenericCBOREntry (ChunkEntry _key2 encoded1) = encodeNamespaced @"blocks/v0" val

      encoded0 `shouldNotBe` encoded1

    it "should roundtrip encode/decode TestEntry successfully" $ do
      val <- genUTxO

      let encoded = encodeNamespaced @"utxo/v0" val
      let decoded = decodeNamespaced @"utxo/v0" @TestEntry encoded

      decoded `shouldBe` (Just (VersionedNS val))
