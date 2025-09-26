{-# LANGUAGE BlockArguments #-}

module Roundtrip (
  tests,
) where

import Cardano.SCLS.CDDL (namespaces)
import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Reader (extractRootHash, withNamespacedData)
import Cardano.SCLS.Internal.Serializer.External.Impl qualified as External (serialize)
import Cardano.SCLS.Internal.Serializer.MemPack
import Cardano.SCLS.Internal.Serializer.Reference.Impl (InputChunk)
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference (serialize)
import Cardano.Types.Network (NetworkId (..))
import Cardano.Types.SlotNo (SlotNo (..))
import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm')
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (
  asMap,
  buildMonoCTree,
  buildRefCTree,
  buildResolvedCTree,
 )
import Codec.CBOR.Cuddle.Huddle
import Codec.CBOR.Term (encodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import Control.Monad (replicateM)
import Crypto.Hash.MerkleTree.Incremental qualified as MT
import Data.Function ((&))
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Streaming.Prelude qualified as S
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Random.Stateful (applyAtomicGen, globalStdGen)
import Test.Hspec
import Test.Hspec.Expectations.Contrib

tests :: Spec
tests =
  describe "Roundtrip test" do
    mkRoundtripTestsFor "Reference" (Reference.serialize @RawBytes)
    mkRoundtripTestsFor "External" (External.serialize @RawBytes)

mkRoundtripTestsFor :: String -> SerializeF -> Spec
mkRoundtripTestsFor groupName serialize =
  describe groupName $ do
    sequence_
      [ context n $ it "should succeed with stream roundtrip" $ roundtrip (T.pack n) (toCDDL huddle)
      | (n, huddle) <- Map.toList namespaces
      ]
 where
  roundtrip namespace cddl = do
    case buildMonoCTree =<< buildResolvedCTree (buildRefCTree $ asMap cddl) of
      Left err -> expectationFailure $ "Failed to build CTree: " ++ show err
      Right mt -> withSystemTempDirectory "scls-format-test-XXXXXX" $ \fn -> do
        data_ <-
          replicateM 1024 $
            applyAtomicGen (generateCBORTerm' mt (Name (T.pack "record_entry") mempty)) globalStdGen
        let encoded_data = [toStrictByteString (encodeTerm term) | term <- data_]
        let fileName = (fn </> "data.scls")
        _ <-
          serialize
            fileName
            Mainnet
            (SlotNo 1)
            (S.each [(namespace S.:> (S.each encoded_data & S.map RawBytes))])
        withNamespacedData
          fileName
          namespace
          ( \stream -> do
              decoded_data <- S.toList_ stream
              annotate
                "Stream roundtrip successful"
                $ [b | RawBytes b <- decoded_data]
                  `shouldBe` (sort encoded_data)
          )
        -- Check roundtrip of root hash
        file_digest <- extractRootHash fileName
        expected_digest <-
          S.each (sort encoded_data)
            & S.fold_ MT.add (MT.empty undefined) (Digest . MT.merkleRootHash . MT.finalize)
        annotate
          "Root hash roundtrip successful"
          $ file_digest
            `shouldBe` (Digest $ MT.merkleRootHash $ MT.finalize $ MT.add (MT.empty undefined) expected_digest)

type SerializeF = FilePath -> NetworkId -> SlotNo -> S.Stream (S.Of (InputChunk RawBytes)) IO () -> IO ()
