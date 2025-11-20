{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.SCLS.Internal.Namespace (
  KnownNamespace (..),
  CanonicalCBOREntryEncoder (..),
  CanonicalCBOREntryDecoder (..),
  VersionedNS (..),
  NamespaceKeySize,
  KnownNamespaceKey (..),
  decodeKey,
) where

import Cardano.SCLS.Internal.Entry (IsKey)
import Cardano.SCLS.Internal.Serializer.MemPack (ByteStringSized (..))
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Data.ByteString (ByteString)
import GHC.TypeLits (KnownNat, Nat, SNat, pattern SNat)

newtype VersionedNS ns a = VersionedNS {unVersionedNS :: a}
  deriving (Eq, Show)

class CanonicalCBOREntryEncoder ns a where
  -- | Encode an entry value to canonical CBOR for the given namespace.
  encodeEntry :: a -> Encoding

class CanonicalCBOREntryDecoder ns a where
  -- | Decode a value from canonical CBOR with a specific namespace.
  decodeEntry :: Decoder s (VersionedNS ns a)

type family NamespaceKeySize ns :: Nat where
  NamespaceKeySize "utxo/v0" = 34
  NamespaceKeySize "blocks/v0" = 32

class KnownNamespaceKey ns where
  namespaceKeySize :: SNat (NamespaceKeySize ns)

instance KnownNamespaceKey "utxo/v0" where
  namespaceKeySize = SNat

instance KnownNamespaceKey "blocks/v0" where
  namespaceKeySize = SNat

decodeKey :: (KnownNamespaceKey ns) => ByteString -> ByteStringSized (NamespaceKeySize ns)
decodeKey bs = ByteStringSized bs

class
  ( IsKey (NamespaceKey ns)
  , KnownNamespaceKey ns
  , KnownNat (NamespaceKeySize ns)
  , Show (NamespaceKey ns)
  , Show (NamespaceEntry ns)
  , CanonicalCBOREntryEncoder ns (NamespaceEntry ns)
  , CanonicalCBOREntryDecoder ns (NamespaceEntry ns)
  ) =>
  KnownNamespace ns
  where
  type NamespaceKey ns
  type NamespaceEntry ns

  encodeKey :: NamespaceKey ns -> ByteStringSized (NamespaceKeySize ns)
