{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module MultiNamespace (
  tests,
) where

import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Reader (extractNamespaceHash, extractNamespaceList, extractRootHash, withNamespacedData)
import Cardano.SCLS.Internal.Serializer.External.Impl qualified as External (serialize)
import Cardano.SCLS.Internal.Serializer.MemPack
import Cardano.SCLS.Internal.Serializer.Reference.Impl (InputChunk)
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference (serialize)
import Cardano.Types.Network (NetworkId (..))
import Cardano.Types.SlotNo (SlotNo (..))
import Crypto.Hash.MerkleTree.Incremental qualified as MT
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Streaming.Prelude qualified as S
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.HUnit
import Test.Hspec
import Test.Hspec.Expectations.Contrib

tests :: Spec
tests =
  describe "Multi-namespace serialization" do
    describe "reference" do
      mkTestsFor (Reference.serialize @RawBytes)
    describe "external" do
      mkTestsFor (External.serialize @RawBytes)

mkTestsFor :: SerializeF -> Spec
mkTestsFor serialize = do
  it "works for a single element in each namespace" do
    roundtrip
      serialize
      [("ns0", ["a"]), ("ns1", ["1"])]
  it "works for multiple elements ordered by namespace" do
    roundtrip
      serialize
      [("ns0", ["a", "b", "c"]), ("ns1", ["1", "2", "3"])]
  it "works for multiple elements mixed order" do
    roundtrip
      serialize
      [("ns0", ["a", "b"]), ("ns1", ["1"]), ("ns0", ["c"]), ("ns1", ["2", "3"])]
  it "works for long sequence" do
    roundtrip
      serialize
      [ ("ns0", [BS8.pack (show (i :: Int)) | i <- [1 .. 2048]])
      , ("ns1", [BS8.pack (show (i :: Int)) | i <- [1 .. 2048]])
      ]

type SerializeF = FilePath -> NetworkId -> SlotNo -> S.Stream (S.Of (InputChunk RawBytes)) IO () -> IO ()

roundtrip :: SerializeF -> [(Text, [ByteString])] -> IO ()
roundtrip serialize input = do
  withSystemTempDirectory "scls-format-test-XXXXXX" $ \fn -> do
    let fileName = fn </> "input.data"
    _ <-
      serialize
        fileName
        Mainnet
        (SlotNo 1)
        mkStream
    nsps <- extractNamespaceList fileName
    annotate "Namespaces are as expected" do
      (Map.keys nsData) `shouldBe` (sort nsps)

    ns_digests <-
      for (Map.toList nsData) \(n, q) -> do
        withNamespacedData
          fileName
          n
          ( \stream -> do
              decoded_data <- S.toList_ stream
              assertEqual
                (T.unpack n <> ": stream roundtrip successful")
                (sort q)
                [b | RawBytes b <- decoded_data]
          )
        fileDigest <- extractNamespaceHash n fileName
        expectedDigest <-
          S.each (sort q)
            & S.fold_ MT.add (MT.empty undefined) (Digest . MT.merkleRootHash . MT.finalize)
        annotate (T.unpack n <> " hash matches expected") do
          fileDigest `shouldBe` (Just expectedDigest)
        pure expectedDigest
    fileDigest <- extractRootHash fileName
    expectedDigest <-
      S.each (ns_digests)
        & S.fold_ MT.add (MT.empty undefined) (Digest . MT.merkleRootHash . MT.finalize)

    annotate "File hash matches expected" do
      fileDigest `shouldBe` expectedDigest
 where
  mkStream =
    S.each
      [ n S.:> (S.each q & S.map RawBytes)
      | (n, q) <- input
      ]
  nsData = Map.fromListWith (<>) input
