{-# LANGUAGE BlockArguments #-}

module CanonicalSpec (
  tests,
) where

import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (fromCanonicalCBOR), Versioned (Versioned))
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (toCanonicalCBOR))
import Cardano.SCLS.Internal.Version (Version (V1))
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
import Data.List (nubBy, sortBy)
import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Word (Word16, Word32, Word64, Word8)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

tests :: Spec
tests =
  describe "Canonical CBOR encoding" do
    let versions = [V1]
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
      )
    prop "encoded map is ordered by encoded key byte-order" $ do
      forAll (genNonDuplicateList (arbitrary @Int) (arbitrary @ByteString)) $
        \list -> do
          let m = Map.fromList list
              sortedList = sortBy (\(k1, _) (k2, _) -> compare (toLazyByteString $ toCanonicalCBOR Proxy k1) (toLazyByteString $ toCanonicalCBOR Proxy k2)) list
              decoded = deserialiseFromBytes (customMapDecoder Proxy) $ toLazyByteString $ toCanonicalCBOR Proxy m
          decoded `shouldBe` Right (BSL.empty, sortedList)
 where
  roundtrip :: forall v a. (Arbitrary a, Typeable a, Show a, Eq a, ToCanonicalCBOR v a, FromCanonicalCBOR v a) => Proxy v -> Proxy a -> SpecWith ()
  roundtrip p p1 = do
    describe (show $ typeRep p1) $ do
      prop "x == decode . encode x" $ do
        \(x :: a) -> do
          let encodedBytes = toLazyByteString $ toCanonicalCBOR p x
              decoded = deserialiseFromBytes (fromCanonicalCBOR @v @a) encodedBytes
          -- Decoded value should match and no bytes left after decoding
          decoded `shouldBe` Right (BSL.empty, Versioned x)
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
    D.decodeMapLenIndef
    D.decodeSequenceLenIndef
      (\acc x -> x : acc)
      []
      reverse -- We prepend, so we must reverse at the end
      do
        Versioned a <- fromCanonicalCBOR @v
        Versioned b <- fromCanonicalCBOR @v
        return (a, b)
