{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tests for namespace-versioned encoding and decoding.

This module tests the namespace-versioned serialization mechanism that
associates data types with specific namespaces and versions.
-}
module NamespacedEncodingSpec where

import Cardano.SCLS.Internal.Namespace (CanonicalCBOREntryDecoder (decodeEntry), CanonicalCBOREntryEncoder (encodeEntry), VersionedNS (VersionedNS))
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import TestEntry

spec :: Spec
spec = do
  describe "CanonicalEncoder" $ do
    it "should encode TestEntry in utxo/v0 namespace" $ do
      NamespacedTestEntry val <- genUTxO
      let _ = encodeEntry @"utxo/v0" val
      -- Just checking if the compiler and type-checker are fine
      pure ()

    it "should encode TestEntry differently in blocks/v0 namespace" $ do
      NamespacedTestEntry val <- genUTxO

      let encodedUTxO = toStrictByteString $ encodeEntry @"utxo/v0" val
      let encodedBlocks = toStrictByteString $ encodeEntry @"blocks/v0" val

      encodedUTxO `shouldNotBe` encodedBlocks

    it "should roundtrip encode/decode TestEntry successfully" $ do
      NamespacedTestEntry val <- genUTxO

      Right (_, decoded) <- pure $ deserialiseFromBytes (decodeEntry @"utxo/v0" @TestEntry) $ toLazyByteString $ encodeEntry @"utxo/v0" val

      decoded `shouldBe` (VersionedNS val)
