module Cardano.SCLS.CBOR.Canonical (
  CanonicalEncoding (unCanonicalEncoding),
  CanonicalDecoder (unCanonicalDecoder),
  unsafeToCanonicalEncoding,
  unsafeToCanonicalDecoder,
) where

import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)

-- | Encoding type wrapper for encodings following deterministic rules.
newtype CanonicalEncoding = CanonicalEncoding {unCanonicalEncoding :: Encoding}
  deriving (Semigroup, Monoid)

{- | Unsafe lifting `Encoding` to `CanonicalEncoding`. Does not ensure that `Encoding` was defined according to canonical rules.
It's the user responsability to ensure the underlying encoding follows deterministic rules.
-}
unsafeToCanonicalEncoding :: Encoding -> CanonicalEncoding
unsafeToCanonicalEncoding = CanonicalEncoding

-- | Decoder type wrapper for decoders following deterministic rules.
newtype CanonicalDecoder s a = CanonicalDecoder {unCanonicalDecoder :: Decoder s a}
  deriving (Functor, Applicative, Monad, MonadFail)

{- | Unsafe lifting `Decoder` to `CanonicalDecoder`. Does not ensure that `Decoder` was defined according to canonical rules.
It's the user responsability to ensure the underlying decoder follows deterministic rules.
-}
unsafeToCanonicalDecoder :: Decoder s a -> CanonicalDecoder s a
unsafeToCanonicalDecoder = CanonicalDecoder
