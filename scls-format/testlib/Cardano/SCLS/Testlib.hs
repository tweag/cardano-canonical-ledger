{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test suite utilities for the implementor
module Cardano.SCLS.Testlib (
  testAllNS,

  -- * Hspec helpers
  testNS,
  validateType,

  -- * properties
  propNamespaceEntryConformsToSpec,
  propNamespaceEntryRoundTrip,
  propTypeConformsToSpec,

  -- * Debug tools
  debugValidateType,
  debugEncodeType,
) where

import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CDDL.Validate
import Cardano.SCLS.Internal.NamespaceCodec
import Codec.CBOR.Cuddle.CBOR.Validator (CBORTermResult (..), CDDLResult (Valid))
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString, toStrictByteString)

-- import Cardano.SCLS.CBOR.Canonical.Encoder

import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BL
import Data.Proxy
import Data.Text qualified as T
import Data.Typeable
import GHC.TypeLits
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

type ConstrNS a = (KnownNamespace a, Arbitrary (NamespaceEntry a), Eq (NamespaceEntry a), Show (NamespaceEntry a))

-- | Test all supported NS for conformance with SCLS.
testAllNS ::
  ( ConstrNS "blocks/v0"
  , ConstrNS "utxo/v0"
  , ConstrNS "pots/v0"
  , ConstrNS "pool_stake/v0"
  , ConstrNS "snapshots/v0"
  , ConstrNS "gov/committee/v0"
  , ConstrNS "gov/constitution/v0"
  , ConstrNS "gov/pparams/v0"
  , ConstrNS "gov/proposals/v0"
  ) =>
  Spec
testAllNS = describe "scls/conformance" $ do
  testNS @"blocks/v0"
  testNS @"utxo/v0"
  testNS @"pots/v0"
  testNS @"pool_stake/v0"
  testNS @"snapshots/v0"
  testNS @"gov/committee/v0"
  testNS @"gov/constitution/v0"
  testNS @"gov/pparams/v0"
  testNS @"gov/proposals/v0"

-- | Validate concrete type against it's definition in CDDL
validateType :: forall ns a. (KnownSymbol ns, ToCanonicalCBOR ns a, Arbitrary a, Show a, Typeable a) => T.Text -> Spec
validateType t = prop ("validate type<" ++ n ++ ">") (propTypeConformsToSpec @ns @a t)
 where
  n = show (typeRep (Proxy @a))

testNS :: forall ns. (KnownSymbol ns, KnownNamespace ns, Arbitrary (NamespaceEntry ns), Eq (NamespaceEntry ns), Show (NamespaceEntry ns)) => Spec
testNS =
  describe nsName $ do
    prop "conforms to spec" $
      propNamespaceEntryConformsToSpec @ns
    prop "canonical with regards to it's definition" $
      propNamespaceEntryRoundTrip @ns
 where
  nsName = (symbolVal (Proxy @ns))

-- | Each value from the known namespace conforms to it's spec
propNamespaceEntryConformsToSpec :: forall ns. (KnownSymbol ns, KnownNamespace ns, Arbitrary (NamespaceEntry ns)) => NamespaceEntry ns -> Bool
propNamespaceEntryConformsToSpec = \a ->
  case validateBytesAgainst (toStrictByteString (encodeEntry @ns a)) nsName "record_entry" of
    Just (CBORTermResult _ Valid{}) -> True
    _ -> False
 where
  nsName = T.pack (symbolVal (Proxy @ns))

propTypeConformsToSpec :: forall ns a. (KnownSymbol ns, ToCanonicalCBOR ns a) => T.Text -> a -> Bool
propTypeConformsToSpec t = \a ->
  case validateBytesAgainst (toStrictByteString (toCanonicalCBOR (Proxy @ns) a)) nsName t of
    Just (CBORTermResult _ Valid{}) -> True
    _ -> False
 where
  nsName = T.pack (symbolVal (Proxy @ns))

{- | Namespace entry are not contradictionary and are canonical with regards
to it's defition: `decode.encode = id`
-}
propNamespaceEntryRoundTrip :: forall ns. (KnownNamespace ns, Arbitrary (NamespaceEntry ns), Eq (NamespaceEntry ns)) => NamespaceEntry ns -> Bool
propNamespaceEntryRoundTrip = \a ->
  case deserialiseFromBytes (decodeEntry @ns) (toLazyByteString (encodeEntry @ns a)) of
    Right (b, Versioned (a' :: NamespaceEntry ns)) ->
      BL.null b
        && case deserialiseFromBytes (decodeEntry @ns) (toLazyByteString (encodeEntry @ns a')) of
          Right (b', Versioned a'') -> BL.null b' && a' == a''
          _ -> False
    _ -> False

debugValidateType :: forall ns a. (KnownSymbol ns, ToCanonicalCBOR ns a) => T.Text -> a -> Maybe CBORTermResult
debugValidateType t a = validateBytesAgainst (toStrictByteString (toCanonicalCBOR (Proxy @ns) a)) nsName t
 where
  nsName = T.pack (symbolVal (Proxy @ns))

-- | Serialize value to CBOR (for usage in debug tools)
debugEncodeType :: forall ns a. (KnownSymbol ns, ToCanonicalCBOR ns a) => a -> B.ByteString
debugEncodeType a = Base16.encode $ toStrictByteString (toCanonicalCBOR (Proxy @ns) a)
