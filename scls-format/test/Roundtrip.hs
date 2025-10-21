{-# LANGUAGE BlockArguments #-}

module Roundtrip (
  tests,
) where

import Cardano.SCLS.CDDL (namespaces)
import Cardano.SCLS.Internal.Entry
import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Reader (extractRootHash, withHeader, withNamespacedData)
import Cardano.SCLS.Internal.Record.Hdr (mkHdr)
import Cardano.SCLS.Internal.Serializer.External.Impl qualified as External (serialize)
import Cardano.SCLS.Internal.Serializer.MemPack
import Cardano.SCLS.Internal.Serializer.Reference.Dump (SerializationPlan, defaultSerializationPlan, withChunks)
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference (serialize)
import Cardano.Types.ByteOrdered
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
import Data.MemPack
import Data.Text qualified as T
import Data.Word (Word32, Word64)
import Streaming.Prelude qualified as S
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Random.Stateful (applyAtomicGen, globalStdGen, uniform)
import Test.Hspec
import Test.Hspec.Expectations.Contrib

data UtxoIn = UtxoIn
  { txId :: Word64 -- Just for simplicity
  , txIx :: Word32
  }
  deriving (Eq, Ord, Show)

instance IsKey UtxoIn where
  keySize = 12
  packKeyM (UtxoIn txId txIx) = do
    packM (BigEndian (fromIntegral txId :: Word64))
    packM (BigEndian (fromIntegral txIx :: Word32))
  unpackKeyM = do
    BigEndian txId <- unpackM
    BigEndian txIx <- unpackM
    return (UtxoIn txId txIx)

tests :: Spec
tests =
  describe "Roundtrip test" do
    mkRoundtripTestsFor "Reference" (Reference.serialize @(ChunkEntry UtxoIn RawBytes))
    mkRoundtripTestsFor "External" (External.serialize @(ChunkEntry UtxoIn RawBytes))

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
        entries <-
          replicateM 1024 $ do
            utxoIn <-
              UtxoIn
                <$> (applyAtomicGen uniform globalStdGen)
                <*> (applyAtomicGen uniform globalStdGen)
            term <- (applyAtomicGen (generateCBORTerm' mt (Name (T.pack "record_entry") mempty)) globalStdGen)
            let encoded_data = toStrictByteString (encodeTerm term)
            pure $ ChunkEntry utxoIn (RawBytes encoded_data)
        -- let encoded_data = [toStrictByteString (encodeTerm term) | term <- data_]
        let fileName = (fn </> "data.scls")
        _ <-
          serialize
            fileName
            Mainnet
            (SlotNo 1)
            (defaultSerializationPlan & withChunks (S.each [namespace S.:> S.each entries]))
        withHeader
          fileName
          ( \hdr ->
              annotate
                "header roundtrip successful"
                $ hdr
                  `shouldBe` mkHdr Mainnet (SlotNo 1)
          )
        withNamespacedData
          fileName
          namespace
          ( \stream -> do
              decoded_data <- S.toList_ stream
              annotate
                "Stream roundtrip successful"
                $ [e | e@ChunkEntry{} <- decoded_data]
                  `shouldBe` (sort entries)
          )
        -- Check roundtrip of root hash
        file_digest <- extractRootHash fileName
        expected_digest <-
          S.each (fmap packByteString $ sort entries)
            & S.fold_ MT.add (MT.empty undefined) (Digest . MT.merkleRootHash . MT.finalize)
        annotate
          "Root hash roundtrip successful"
          $ file_digest
            `shouldBe` (Digest $ MT.merkleRootHash $ MT.finalize $ MT.add (MT.empty undefined) expected_digest)

type SerializeF = FilePath -> NetworkId -> SlotNo -> SerializationPlan (ChunkEntry UtxoIn RawBytes) -> IO ()
