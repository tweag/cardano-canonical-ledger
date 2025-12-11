module Cardano.SCLS.CBOR.Canonical (
  CanonicalEncoding (getRawEncoding),
  CanonicalDecoder (getRawDecoder),
  assumeCanonicalEncoding,
  assumeCanonicalDecoder,
) where

import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)

-- | Encoding type wrapper for encodings following deterministic rules.
newtype CanonicalEncoding = CanonicalEncoding {getRawEncoding :: Encoding}
  deriving (Semigroup, Monoid)

{- | Unchecked lifting from `Encoding` to `CanonicalEncoding`.
It's the user responsibility to ensure the underlying encoding follows deterministic rules.
-}
assumeCanonicalEncoding :: Encoding -> CanonicalEncoding
assumeCanonicalEncoding = CanonicalEncoding

-- | Decoder type wrapper for decoders following deterministic rules.
newtype CanonicalDecoder s a = CanonicalDecoder {getRawDecoder :: Decoder s a}
  deriving (Functor, Applicative, Monad, MonadFail)

{- | Unchecked lifting from `Decoder` to `CanonicalDecoder`.
It's the user responsibility to ensure the underlying decoder follows deterministic rules.
-}
assumeCanonicalDecoder :: Decoder s a -> CanonicalDecoder s a
assumeCanonicalDecoder = CanonicalDecoder
