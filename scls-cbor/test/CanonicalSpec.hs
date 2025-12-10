{-# LANGUAGE BlockArguments #-}

module CanonicalSpec (
  tests,
) where

import Cardano.SCLS.CBOR.Canonical (CanonicalDecoder (unCanonicalDecoder), CanonicalEncoding (unCanonicalEncoding))
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (fromCanonicalCBOR))
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (toCanonicalCBOR))
import Cardano.SCLS.Versioned
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short (ShortByteString)
import Data.Data (Typeable, typeRep)
import Data.Int
import Data.List (nubBy, sortOn)
import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word (Word16, Word32, Word64, Word8)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

tests :: Spec
tests =
  describe "Canonical CBOR encoding" do
    let versions = ["base/v0"]
    forM_
      versions
      ( \v ->
          describe (show v) $ do
            roundtrip Proxy (Proxy @())
            roundtrip Proxy (Proxy @Integer)
            roundtrip Proxy (Proxy @Word)
            roundtrip Proxy (Proxy @Word8)
            roundtrip Proxy (Proxy @Word16)
            roundtrip Proxy (Proxy @Word32)
            roundtrip Proxy (Proxy @Word64)
            roundtrip Proxy (Proxy @Int)
            roundtrip Proxy (Proxy @Int32)
            roundtrip Proxy (Proxy @Int64)
            roundtrip Proxy (Proxy @ByteString)
            roundtrip Proxy (Proxy @ShortByteString)
            roundtrip Proxy (Proxy @(Int, ByteString))
            roundtrip Proxy (Proxy @[Int])
            roundtrip Proxy (Proxy @(Seq.Seq Int))
            roundtrip Proxy (Proxy @Bool)
            roundtrip Proxy (Proxy @(Map.Map Int ByteString))
            roundtrip Proxy (Proxy @(Set.Set ByteString))
            roundtripWith Proxy (Proxy @T.Text) T.pack
      )
    prop "encoded map is ordered by encoded key byte-order" $
      forAll (genNonDuplicateList (arbitrary @Int) (arbitrary @ByteString)) $
        \list ->
          let m = Map.fromList list
              sortedList = sortOn (toLazyByteString . unCanonicalEncoding . toCanonicalCBOR Proxy . fst) list
              decoded = deserialiseFromBytes (customMapDecoder Proxy) $ toLazyByteString $ unCanonicalEncoding $ toCanonicalCBOR Proxy m
           in decoded `shouldBe` Right (BSL.empty, sortedList)
 where
  roundtrip :: forall v a. (Arbitrary a, Typeable a, Show a, Eq a, ToCanonicalCBOR v a, FromCanonicalCBOR v a) => Proxy v -> Proxy a -> SpecWith ()
  roundtrip p p1 = roundtripWith p p1 id
  roundtripWith :: forall v a x. (Arbitrary a, Typeable x, Show a, Show x, Eq x, ToCanonicalCBOR v x, FromCanonicalCBOR v x) => Proxy v -> Proxy x -> (a -> x) -> SpecWith ()
  roundtripWith p p1 f = do
    describe (show $ typeRep p1) $ do
      prop "x == decode . encode x" $ do
        \(x :: a) -> do
          let encodedBytes = toLazyByteString $ unCanonicalEncoding $ toCanonicalCBOR p (f x)
              decoded = deserialiseFromBytes (unCanonicalDecoder $ fromCanonicalCBOR @v @x) encodedBytes
          -- Decoded value should match and no bytes left after decoding
          decoded `shouldBe` Right (BSL.empty, Versioned (f x))
  -- Generate list of pairs with no duplicate first element
  genNonDuplicateList :: (Eq a) => Gen a -> Gen b -> Gen [(a, b)]
  genNonDuplicateList g1 g2 =
    nubBy (\(x, _) (y, _) -> x == y) <$> listOf g
   where
    g = do
      v1 <- g1
      v2 <- g2
      pure (v1, v2)
  -- Decode map as list of pairs
  customMapDecoder :: forall v s a b. (FromCanonicalCBOR v a, FromCanonicalCBOR v b) => Proxy v -> Decoder s ([(a, b)])
  customMapDecoder _ = do
    len <- D.decodeMapLenCanonical
    D.decodeSequenceLenN
      (\acc x -> x : acc)
      []
      reverse -- We prepend, so we must reverse at the end
      len
      ( unCanonicalDecoder $ do
          Versioned a <- fromCanonicalCBOR @v
          Versioned b <- fromCanonicalCBOR @v
          return (a, b)
      )
