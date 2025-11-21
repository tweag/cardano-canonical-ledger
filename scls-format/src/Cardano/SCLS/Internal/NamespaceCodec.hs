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
  encodeKeyToBytes,
  decodeKeyFromBytes,
) where

import Cardano.SCLS.Internal.Entry.IsKey (IsKey (keySize, packKeyM, unpackKeyM))
import Cardano.SCLS.Internal.Serializer.MemPack (ByteStringSized (..))
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Fail (runFailLastT)
import Data.ByteString (ByteString)
import Data.Data (Proxy (Proxy), Typeable, typeRep)
import Data.MemPack (StateT (runStateT), Unpack (runUnpack), packWithByteArray)
import Data.MemPack.Buffer (pinnedByteArrayToByteString)
import GHC.TypeLits (KnownNat, Nat, Symbol, fromSNat, pattern SNat)

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

Complements 'CanonicalCBOREntryEncoder' for deserialization . Returns a 'VersionedNS'
wrapper that tracks the namespace information.
-}
class CanonicalCBOREntryDecoder ns a where
  -- | Decode a value from canonical CBOR with a specific namespace.
  decodeEntry :: Decoder s (VersionedNS ns a)

{- | Maps a namespace (represented as a type-level string/Symbol) to its expected key size.

Instances for this type family should be created for each namespace/version.

For example:
@
type instance NamespaceKeySize "utxo/v0" = 34
@
-}
type family NamespaceKeySize (ns :: Symbol) :: Nat

-- | Get the namespace key size at runtime.
namespaceKeySize :: forall ns. (KnownNat (NamespaceKeySize ns)) => Int
namespaceKeySize =
  fromInteger $ fromSNat $ SNat @(NamespaceKeySize ns)

{- | A type class that associates a namespace with its key and entry types.
This type class acts as a bridge between the namespace/version, its key type,
its entry type, and the encoding/decoding mechanisms.

Defining an instance of 'KnownNamespace' for a specific namespace requires specifying:
  - 'NamespaceKey': The type used as the key for entries in this namespace.
  - 'NamespaceEntry': The type of the entries stored in this namespace.
  - 'CanonicalCBOREntryEncoder' and 'CanonicalCBOREntryDecoder' instances for the entry type.
  - 'NamespaceKeySize' type family instance for the namespace.
  - 'IsKey' instance for the key type.
-}
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

-- | Encode a namespace key to a 'ByteStringSized' of the appropriate size.
encodeKeyToBytes :: forall ns. (KnownNamespace ns, Typeable (NamespaceKey ns)) => NamespaceKey ns -> ByteStringSized (NamespaceKeySize ns)
encodeKeyToBytes key =
  ByteStringSized . pinnedByteArrayToByteString $ packWithByteArray True (show (typeRep (Proxy @(NamespaceKey ns)))) (keySize @(NamespaceKey ns)) (packKeyM key)

{- | Decode a namespace key from a 'ByteString'.
Returns 'Nothing' if decoding fails.
-}
decodeKeyFromBytes ::
  forall ns.
  ( KnownNamespace ns
  , Typeable (NamespaceKey ns)
  ) =>
  Proxy ns ->
  ByteString ->
  Maybe (NamespaceKey ns)
decodeKeyFromBytes _ bs = do
  let unpacker = unpackKeyM
  either (const Nothing) (Just . fst) $ runST $ runFailLastT $ runStateT (runUnpack unpacker bs) 0
