{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

{- | Canonical CBOR representation

This module contains only instances for cases where there is a direct and
unambiguous instance that will remain the same across all versions. For
example, since there are multiple possible ways to encode 'Maybe' we do
not provide an instance here - it is better to specify the precise encoding
when defining the instances for specific types.
-}
module Cardano.SCLS.CBOR.Canonical.Encoder where

import Codec.CBOR.ByteArray.Sliced qualified as BAS
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Encoding qualified as E
import Codec.CBOR.Write (toStrictByteString)
import Data.Array.Byte qualified as Prim
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString (SBS))
import Data.ByteString.Short qualified as SBS
import Data.Int
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Word
import GHC.TypeLits

-- | Encode data to CBOR corresponding with the SCLS format.
class ToCanonicalCBOR (v :: Symbol) a where
  -- | Encode to canonical CBOR at a given version
  toCanonicalCBOR :: proxy v -> a -> Encoding

--------------------------------------------------------------------------------
-- Encoding, Term etc
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v Encoding where
  toCanonicalCBOR _ = id

--------------------------------------------------------------------------------
-- Primitive types
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v () where
  toCanonicalCBOR _ = const E.encodeNull

instance ToCanonicalCBOR v Bool where
  toCanonicalCBOR _ = E.encodeBool

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v Integer where
  toCanonicalCBOR _ = E.encodeInteger

instance ToCanonicalCBOR v Word where
  toCanonicalCBOR _ = E.encodeWord

instance ToCanonicalCBOR v Word8 where
  toCanonicalCBOR _ = E.encodeWord8

instance ToCanonicalCBOR v Word16 where
  toCanonicalCBOR _ = E.encodeWord16

instance ToCanonicalCBOR v Word32 where
  toCanonicalCBOR _ = E.encodeWord32

instance ToCanonicalCBOR v Word64 where
  toCanonicalCBOR _ = E.encodeWord64

instance ToCanonicalCBOR v Int where
  toCanonicalCBOR _ = E.encodeInt

instance ToCanonicalCBOR v Int32 where
  toCanonicalCBOR _ = E.encodeInt32

instance ToCanonicalCBOR v Int64 where
  toCanonicalCBOR _ = E.encodeInt64

--------------------------------------------------------------------------------
-- Bytes
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v ByteString where
  toCanonicalCBOR _ = E.encodeBytes

instance ToCanonicalCBOR v SBS.ShortByteString where
  toCanonicalCBOR _ sbs@(SBS ba) =
    E.encodeByteArray $ BAS.SBA (Prim.ByteArray ba) 0 (SBS.length sbs)

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v Text where
  toCanonicalCBOR _ = E.encodeString

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

instance
  (ToCanonicalCBOR v a, ToCanonicalCBOR v b) =>
  ToCanonicalCBOR v (a, b)
  where
  toCanonicalCBOR v (a, b) =
    E.encodeListLen 2
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  ) =>
  ToCanonicalCBOR v (a, b, c)
  where
  toCanonicalCBOR v (a, b, c) =
    E.encodeListLen 3
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  , ToCanonicalCBOR v d
  ) =>
  ToCanonicalCBOR v (a, b, c, d)
  where
  toCanonicalCBOR v (a, b, c, d) =
    E.encodeListLen 4
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c
      <> toCanonicalCBOR v d

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  , ToCanonicalCBOR v d
  , ToCanonicalCBOR v e
  ) =>
  ToCanonicalCBOR v (a, b, c, d, e)
  where
  toCanonicalCBOR v (a, b, c, d, e) =
    E.encodeListLen 5
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c
      <> toCanonicalCBOR v d
      <> toCanonicalCBOR v e

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  , ToCanonicalCBOR v d
  , ToCanonicalCBOR v e
  , ToCanonicalCBOR v f
  ) =>
  ToCanonicalCBOR v (a, b, c, d, e, f)
  where
  toCanonicalCBOR v (a, b, c, d, e, f) =
    E.encodeListLen 6
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c
      <> toCanonicalCBOR v d
      <> toCanonicalCBOR v e
      <> toCanonicalCBOR v f

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  , ToCanonicalCBOR v d
  , ToCanonicalCBOR v e
  , ToCanonicalCBOR v f
  , ToCanonicalCBOR v g
  ) =>
  ToCanonicalCBOR v (a, b, c, d, e, f, g)
  where
  toCanonicalCBOR v (a, b, c, d, e, f, g) =
    E.encodeListLen 7
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c
      <> toCanonicalCBOR v d
      <> toCanonicalCBOR v e
      <> toCanonicalCBOR v f
      <> toCanonicalCBOR v g

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  , ToCanonicalCBOR v d
  , ToCanonicalCBOR v e
  , ToCanonicalCBOR v f
  , ToCanonicalCBOR v g
  , ToCanonicalCBOR v h
  ) =>
  ToCanonicalCBOR v (a, b, c, d, e, f, g, h)
  where
  toCanonicalCBOR v (a, b, c, d, e, f, g, h) =
    E.encodeListLen 8
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c
      <> toCanonicalCBOR v d
      <> toCanonicalCBOR v e
      <> toCanonicalCBOR v f
      <> toCanonicalCBOR v g
      <> toCanonicalCBOR v h

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

-- | We always encode lists with the definite length encoding.
instance (ToCanonicalCBOR v a) => ToCanonicalCBOR v [a] where
  toCanonicalCBOR v xs =
    foldl' (\r x -> r <> toCanonicalCBOR v x) (E.encodeListLen (fromIntegral $ length xs)) xs

instance (ToCanonicalCBOR v a) => ToCanonicalCBOR v (Seq.Seq a) where
  toCanonicalCBOR v xs =
    foldl' (\r x -> r <> toCanonicalCBOR v x) (E.encodeListLen (fromIntegral $ length xs)) xs

--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

-- | We always encode maps with the definite length encoding and ordered by encoded keys byte-order.
instance
  (ToCanonicalCBOR v k, ToCanonicalCBOR v val) =>
  ToCanonicalCBOR v (Map.Map k val)
  where
  toCanonicalCBOR v m =
    foldl'
      (\b (k, val) -> b <> E.encodePreEncoded k <> toCanonicalCBOR v val)
      (E.encodeMapLen (fromIntegral $ length mSorted))
      mSorted
   where
    -- Order map by the byte-wise ordering of the canonically encoded map keys
    mSorted =
      sortOn fst $
        Map.foldlWithKey'
          ( \acc k val ->
              let keyBytes = toStrictByteString $ toCanonicalCBOR v k
               in (keyBytes, val) : acc
          )
          []
          m
