{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Roundtrip (
  tests,
) where

import Cardano.SCLS.CDDL (NamespaceInfo (..), namespaces)
import Cardano.SCLS.Internal.Entry.CBOREntry (GenericCBOREntry (GenericCBOREntry), SomeCBOREntry (SomeCBOREntry))
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (ChunkEntry))
import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Reader (extractRootHash, withHeader, withNamespacedData, withRecordData)
import Cardano.SCLS.Internal.Record.Hdr (mkHdr)
import Cardano.SCLS.Internal.Record.Metadata (Metadata (..), MetadataEntry (MetadataEntry))
import Cardano.SCLS.Internal.Serializer.Dump.Plan (SerializationPlan, addChunks, addMetadata, defaultSerializationPlan)
import Cardano.SCLS.Internal.Serializer.External.Impl qualified as External (serialize)
import Cardano.SCLS.Internal.Serializer.HasKey (sortByKey)
import Cardano.SCLS.Internal.Serializer.MemPack
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference (serialize)
import Cardano.Types.Namespace qualified as Namespace
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
import Codec.CBOR.Read
import Codec.CBOR.Term
import Codec.CBOR.Write
import Control.Monad (replicateM)
import Crypto.Hash.MerkleTree.Incremental qualified as MT
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.MemPack
import Data.Text qualified as T
import GHC.TypeNats
import Streaming.Prelude qualified as S
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Random.Stateful (applyAtomicGen, globalStdGen, uniformByteStringM)
import Test.Hspec
import Test.Hspec.Expectations.Contrib

tests :: Spec
tests =
  describe "Roundtrip test" do
    mkRoundtripTestsFor "Reference" (Reference.serialize @SomeCBOREntry)
    mkRoundtripTestsFor "External" (External.serialize @SomeCBOREntry)

mkRoundtripTestsFor :: String -> SerializeF -> Spec
mkRoundtripTestsFor groupName serialize =
  describe groupName $ do
    sequence_
      [ context (Namespace.asString n) $ it "should succeed with stream roundtrip" $ roundtrip n (namespaceKeySize ns, toCDDL (namespaceSpec ns))
      | (Namespace.fromText -> n, ns) <- Map.toList namespaces
      ]
 where
  roundtrip namespace (kSize, cddl) = do
    case buildMonoCTree =<< buildResolvedCTree (buildRefCTree $ asMap cddl) of
      Left err -> expectationFailure $ "Failed to build CTree: " ++ show err
      Right mt -> withSystemTempDirectory "scls-format-test-XXXXXX" $ \fn -> do
        entries <-
          withSomeSNat kSize \(snat :: SNat n) -> do
            withKnownNat snat do
              replicateM 1024 $ do
                key <- uniformByteStringM (fromIntegral kSize) globalStdGen
                term <- applyAtomicGen (generateCBORTerm' mt (Name (T.pack "record_entry") mempty)) globalStdGen
                Right (_, canonicalTerm) <- pure $ deserialiseFromBytes decodeTerm $ toLazyByteString (encodeTerm term)
                pure $! SomeCBOREntry (GenericCBOREntry $ ChunkEntry (ByteStringSized @n key) (CBORTerm canonicalTerm))
        mEntries <-
          replicateM 1024 $ do
            MetadataEntry
              <$> (uniformByteStringM 20 globalStdGen)
              <*> (uniformByteStringM 100 globalStdGen)
        let fileName = (fn </> "data.scls")
        _ <-
          serialize
            fileName
            Mainnet
            (SlotNo 1)
            ( defaultSerializationPlan
                & addChunks (S.each [namespace S.:> S.each entries])
                & addMetadata (S.each mEntries)
            )
        withHeader
          fileName
          ( \hdr ->
              annotate
                "header roundtrip successful"
                $ hdr
                  `shouldBe` mkHdr Mainnet (SlotNo 1)
          )
        withSomeSNat kSize \(snat :: SNat n) -> do
          withKnownNat snat do
            withNamespacedData
              fileName
              namespace
              ( \stream -> do
                  decoded_data <- S.toList_ stream
                  annotate
                    "Stream roundtrip successful"
                    $ [SomeCBOREntry e | (e :: GenericCBOREntry n) <- decoded_data]
                      `shouldBe` (sortByKey entries)
              )
        -- Check roundtrip of root hash
        file_digest <- extractRootHash fileName
        expected_digest <-
          S.each (fmap packByteString $ sortByKey entries)
            & S.fold_ MT.add (MT.empty undefined) (Digest . MT.merkleRootHash . MT.finalize)
        annotate
          "Root hash roundtrip successful"
          $ file_digest
            `shouldBe` (Digest $ MT.merkleRootHash $ MT.finalize $ MT.add (MT.empty undefined) expected_digest)

        withRecordData
          fileName
          ( \stream -> do
              decoded_metadata <- S.toList_ stream
              annotate
                "Metadata stream roundtrip successful"
                $ mconcat [metadataEntries | Metadata{metadataEntries} <- decoded_metadata]
                  `shouldBe` mEntries
          )

type SerializeF = FilePath -> NetworkId -> SlotNo -> SerializationPlan SomeCBOREntry -> IO ()
