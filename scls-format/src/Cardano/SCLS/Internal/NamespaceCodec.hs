{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- | Namespace-versioned canonical CBOR encoding/decoding.

This module provides a mechanism to associate data types with versioned namespaces
at the type level. This is useful for handling different eras of Cardano ledger,
where each era might have different data structures with their own encoding schemes.
-}
module Cardano.SCLS.Internal.NamespaceCodec (
  KnownNamespace (..),
  CanonicalCBOREntryEncoder (..),
  CanonicalCBOREntryDecoder (..),
  VersionedNS (..),
  NamespaceKeySize,
  namespaceKeySize,
  encodeKey,
) where

import Cardano.SCLS.Internal.Entry.IsKey (IsKey (keySize, packKeyM))
import Cardano.SCLS.Internal.Serializer.MemPack (ByteStringSized (..))
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Data.Data (Proxy (Proxy), Typeable, typeRep)
import Data.MemPack (packWithByteArray)
import Data.MemPack.Buffer (pinnedByteArrayToByteString)
import GHC.TypeLits (KnownNat, Nat, fromSNat, pattern SNat)

-- | A wrapper type that associates a decoded value with its namespace.
newtype VersionedNS ns a = VersionedNS {unVersionedNS :: a}
  deriving (Eq, Show)

{- | Encode a value to canonical CBOR with a specific namespace.

A namespace identifies the specific version of the encoding for a data type.
For example, "utxo/v0" might represent a certain era's UTxO format, while
"utxo/v1" represents the following era's format.

The namespace parameter is a type-level string (Symbol) that constrains the
encoding at compile-time. The key size is automatically determined by the
'NamespaceKeySize' type family based on the namespace.
-}
class CanonicalCBOREntryEncoder ns a where
  -- | Encode an entry value to canonical CBOR for the given namespace.
  encodeEntry :: a -> Encoding

{- | Decode a value from canonical CBOR with a specific namespace.

Complements 'CanonicalEncoder' for deserialization. Returns a 'VersionedNS'
wrapper that tracks the namespace information.
-}
class CanonicalCBOREntryDecoder ns a where
  -- | Decode a value from canonical CBOR with a specific namespace.
  decodeEntry :: Decoder s (VersionedNS ns a)

{- | Maps a namespace (represented as a type-level string/Symbol) to its expected key size.

This type family should be updated whenever new namespaces are added to the system,
and should remain in sync with the runtime namespace registry in Cardano.SCLS.CDDL.
-}
type family NamespaceKeySize ns :: Nat where
  NamespaceKeySize "utxo/v0" = 34
  NamespaceKeySize "blocks/v0" = 32

-- | Get the namespace key size at runtime.
namespaceKeySize :: forall ns. (KnownNat (NamespaceKeySize ns)) => Int
namespaceKeySize =
  fromInteger $ fromSNat $ SNat @(NamespaceKeySize ns)

class
  ( IsKey (NamespaceKey ns)
  , KnownNat (NamespaceKeySize ns) -- ensures type family NamespaceKeySize is defined for this namespace
  , CanonicalCBOREntryEncoder ns (NamespaceEntry ns)
  , CanonicalCBOREntryDecoder ns (NamespaceEntry ns)
  ) =>
  KnownNamespace ns
  where
  type NamespaceKey ns
  type NamespaceEntry ns

encodeKey :: forall ns. (KnownNamespace ns, Typeable (NamespaceKey ns)) => NamespaceKey ns -> ByteStringSized (NamespaceKeySize ns)
encodeKey key =
  ByteStringSized . pinnedByteArrayToByteString $ packWithByteArray True (show (typeRep (Proxy @(NamespaceKey ns)))) (keySize @(NamespaceKey ns)) (packKeyM key)
