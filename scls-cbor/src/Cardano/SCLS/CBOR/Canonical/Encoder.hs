{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

{- | Canonical CBOR representation

This module contains only instances for cases where there is a direct and
unambiguous instance that will remain the same across all versions. For
example, since there are multiple possible ways to encode 'Maybe' we do
not provide an instance here - it is better to specify the precise encoding
when defining the instances for specific types.
-}
module Cardano.SCLS.CBOR.Canonical.Encoder (
  ToCanonicalCBOR (..),
  encodeAsMap,
  SomeEncodablePair (..),
  mkEncodablePair,
) where

import Cardano.SCLS.CBOR.Canonical (CanonicalEncoding (unCanonicalEncoding), unsafeToCanonicalEncoding)
import Codec.CBOR.ByteArray.Sliced qualified as BAS
import Codec.CBOR.Encoding qualified as E
import Codec.CBOR.Write (toStrictByteString)
import Data.Array.Byte qualified as Prim
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString (SBS))
import Data.ByteString.Short qualified as SBS
import Data.Foldable qualified as F
import Data.Int
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word
import GHC.TypeLits

-- | Encode data to CBOR corresponding with the SCLS format.
class ToCanonicalCBOR (v :: Symbol) a where
  -- | Encode to canonical CBOR at a given version
  toCanonicalCBOR :: proxy v -> a -> CanonicalEncoding

--------------------------------------------------------------------------------
-- Encoding, Term etc
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v CanonicalEncoding where
  toCanonicalCBOR _ = id

--------------------------------------------------------------------------------
-- Primitive types
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v () where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . const E.encodeNull

instance ToCanonicalCBOR v Bool where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . E.encodeBool

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v Integer where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . E.encodeInteger

instance ToCanonicalCBOR v Word where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . E.encodeWord

instance ToCanonicalCBOR v Word8 where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . E.encodeWord8

instance ToCanonicalCBOR v Word16 where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . E.encodeWord16

instance ToCanonicalCBOR v Word32 where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . E.encodeWord32

instance ToCanonicalCBOR v Word64 where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . E.encodeWord64

instance ToCanonicalCBOR v Int where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . E.encodeInt

instance ToCanonicalCBOR v Int32 where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . E.encodeInt32

instance ToCanonicalCBOR v Int64 where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . E.encodeInt64

--------------------------------------------------------------------------------
-- Bytes
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v ByteString where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . E.encodeBytes

instance ToCanonicalCBOR v SBS.ShortByteString where
  toCanonicalCBOR _ sbs@(SBS ba) =
    unsafeToCanonicalEncoding $
      E.encodeByteArray $
        BAS.SBA (Prim.ByteArray ba) 0 (SBS.length sbs)

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v Text where
  toCanonicalCBOR _ = unsafeToCanonicalEncoding . E.encodeString

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

instance
  (ToCanonicalCBOR v a, ToCanonicalCBOR v b) =>
  ToCanonicalCBOR v (a, b)
  where
  toCanonicalCBOR v (a, b) =
    unsafeToCanonicalEncoding (E.encodeListLen 2)
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
    unsafeToCanonicalEncoding (E.encodeListLen 3)
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
    unsafeToCanonicalEncoding (E.encodeListLen 4)
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
    unsafeToCanonicalEncoding (E.encodeListLen 5)
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
    unsafeToCanonicalEncoding (E.encodeListLen 6)
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
    unsafeToCanonicalEncoding (E.encodeListLen 7)
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
    unsafeToCanonicalEncoding (E.encodeListLen 8)
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
    (unsafeToCanonicalEncoding (E.encodeListLen (fromIntegral $ length xs))) <> foldMap (toCanonicalCBOR v) xs

instance (ToCanonicalCBOR v a) => ToCanonicalCBOR v (Seq.Seq a) where
  toCanonicalCBOR v xs =
    (unsafeToCanonicalEncoding (E.encodeListLen (fromIntegral $ length xs))) <> foldMap (toCanonicalCBOR v) xs

--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

-- | We always encode maps with the definite length encoding and ordered by encoded keys byte-order.
instance
  (ToCanonicalCBOR v k, ToCanonicalCBOR v val) =>
  ToCanonicalCBOR v (Map.Map k val)
  where
  toCanonicalCBOR v m =
    encodeAsMap l
   where
    l =
      Map.foldlWithKey'
        (\acc k val -> SomeEncodablePair v k val : acc)
        []
        m

{- | An existential wrapper for a key-value pair where both the key and value
can be canonically CBOR-encoded under the version @v@.

This type is useful for encoding heterogeneous collections of key-value pairs
as CBOR maps, where the key and value types may vary but all support
'ToCanonicalCBOR' for the same version.

Use 'mkEncodablePair' to construct values of this type.
-}
data SomeEncodablePair v where
  SomeEncodablePair :: (ToCanonicalCBOR v k, ToCanonicalCBOR v val) => proxy v -> k -> val -> SomeEncodablePair v

{- | Construct a 'SomeEncodablePair' from a key and value, given that both
support 'ToCanonicalCBOR' for the version `v`.
-}
mkEncodablePair :: forall v k val. (ToCanonicalCBOR v k, ToCanonicalCBOR v val) => k -> val -> SomeEncodablePair v
mkEncodablePair = SomeEncodablePair (Proxy @v)

{- |
  Helper for encoding map-like structures in canonical CBOR form.
  This function takes a foldable collection of 'SomeEncodablePair' values and encodes them as a CBOR map,
  using definite length encoding. Keys are sorted by their canonical CBOR-encoded byte representation,
  as required by the canonical CBOR specification.
-}
encodeAsMap :: (Foldable t) => t (SomeEncodablePair v) -> CanonicalEncoding
encodeAsMap f =
  (unsafeToCanonicalEncoding (E.encodeMapLen len))
    <> foldMap
      (\(kBytes, valEncoding) -> unsafeToCanonicalEncoding (E.encodePreEncoded kBytes) <> valEncoding)
      sorted
 where
  -- Order map by the byte-wise ordering of the canonically encoded map keys
  (len, l) =
    F.foldl'
      ( \(n, acc) (SomeEncodablePair v k val) ->
          let kBytes = toStrictByteString $ unCanonicalEncoding $ toCanonicalCBOR v k
              valEncoding = toCanonicalCBOR v val
           in (n + 1, (kBytes, valEncoding) : acc)
      )
      (0, [])
      f
  sorted = List.sortOn fst l

--------------------------------------------------------------------------------
-- Set
--------------------------------------------------------------------------------

-- See `https://github.com/input-output-hk/cbor-sets-spec/blob/master/CBOR_SETS.md`
-- for details about the implementation.
instance (ToCanonicalCBOR v a) => (ToCanonicalCBOR v (Set.Set a)) where
  toCanonicalCBOR v s =
    unsafeToCanonicalEncoding $
      E.encodeTag 258
        <> E.encodeListLen (fromIntegral size)
        <> foldMap E.encodePreEncoded encSorted
   where
    size = Set.size s
    encSorted = List.sort $ map (toStrictByteString . unCanonicalEncoding . toCanonicalCBOR v) $ Set.toList s
