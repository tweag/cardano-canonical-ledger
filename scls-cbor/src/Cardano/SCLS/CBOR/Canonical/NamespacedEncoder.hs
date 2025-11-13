{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

{- | Namespace-versioned canonical CBOR encoding/decoding.

This module provides a mechanism to associate data types with versioned namespaces
at the type level. This is useful for handling different eras of Cardano ledger,
where each era might have different data structures with their own encoding schemes.
-}
module Cardano.SCLS.CBOR.Canonical.NamespacedEncoder (
  VersionedNS (..),
  CanonicalEncoder (..),
  CanonicalDecoder (..),
  KnownNamespaceKeySize (..),
  NamespaceKeySize,
) where

import Cardano.SCLS.Internal.Entry (GenericCBOREntry)
import Data.Kind (Type)
import GHC.TypeLits (Nat, SNat, Symbol)
import GHC.TypeNats (pattern SNat)

{- | Maps a namespace (represented as a type-level string/Symbol) to its expected key size.

This type family should be updated whenever new namespaces are added to the system,
and should remain in sync with the runtime namespace registry in Cardano.SCLS.CDDL.
-}
type family NamespaceKeySize (ns :: Symbol) :: Nat where
  NamespaceKeySize "utxo/v0" = 34
  NamespaceKeySize "blocks/v0" = 32

{- | Type class that enforces a namespace has a registered key size.

This class is used to ensure that only registered namespaces can be used
with 'CanonicalEncoder' and 'CanonicalDecoder'.
-}
class KnownNamespaceKeySize (ns :: Symbol) where
  -- | Get the namespace key size at runtime.
  namespaceKeySize :: SNat (NamespaceKeySize ns)

{- Instances for all known namespaces

As long as `NamespaceKeySize` is defined for a certain namespace, instances of this class type should all be the same.
-}
instance KnownNamespaceKeySize "utxo/v0" where
  namespaceKeySize = SNat

instance KnownNamespaceKeySize "blocks/v0" where
  namespaceKeySize = SNat

-- | A wrapper type that associates a decoded value with its namespace.
newtype VersionedNS (ns :: Symbol) a = VersionedNS {unVersionedNS :: a}
  deriving (Eq, Show)

{- | Encode a value to canonical CBOR with a specific namespace.

A namespace identifies the specific version of the encoding for a data type.
For example, "utxo/v0" might represent the certain era's UTxO format, while
"utxo/v1" represents the following era's format.

The namespace parameter is a type-level string (Symbol) that constrains the
encoding at compile-time. The key size is automatically determined by the
'NamespaceKeySize' type family based on the namespace.

Requires a 'KnownNamespaceKeySize' instance, which ensures the namespace
is registered at compile time.
-}
class (KnownNamespaceKeySize ns) => CanonicalEncoder ns (a :: Type) where
  -- | Encode a value to canonical CBOR for the given namespace.
  encodeNamespaced :: a -> GenericCBOREntry (NamespaceKeySize ns)

{- | Decode a value from canonical CBOR with a specific namespace.

Complements 'CanonicalEncoder' for deserialization. Returns a 'VersionedNS'
wrapper that tracks the namespace information.
-}
class (KnownNamespaceKeySize ns) => CanonicalDecoder (ns :: Symbol) (a :: Type) where
  {- | Decode a value from canonical CBOR for the given namespace.

  Returns the decoded value wrapped in 'VersionedNS' to track the namespace.
  -}
  decodeNamespaced :: GenericCBOREntry (NamespaceKeySize ns) -> Maybe (VersionedNS ns a)
