module MetadataBuilderSpec (metadataBuilderTests) where

import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Record.Metadata
import Cardano.SCLS.Internal.Serializer.MetadataBuilder.InMemory
import Control.Monad
import Data.ByteString qualified as BS
import Data.Maybe
import Data.MemPack
import Data.MemPack.Extra (Entry (Entry))
import Data.Primitive.ByteArray
import Test.HUnit
import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Test.Hspec.QuickCheck
import Test.QuickCheck

metadataBuilderTests :: Spec
metadataBuilderTests =
  describe "MetadataBuilder.InMemory" $ do
    bufferBoundaryTests
    finalizationTests

genMetadataEntry :: Gen MetadataEntry
genMetadataEntry = do
  subjectSize <- choose (0, 50)
  valueSize <- choose (0, 100)
  let subject = (BS.replicate subjectSize 0x41)
  let value = (BS.replicate valueSize 0x42)
  return $ MetadataEntry subject value

genSizedMetadataEntry :: Int -> Gen MetadataEntry
genSizedMetadataEntry size = do
  let size' = size - 8 - 4 -- account for 4 bytes subject length and 4 bytes value length
  subjectSize <- choose (0, size' `div` 4)
  let valueSize = size' - subjectSize
  let subject = (BS.replicate subjectSize 0x41)
  let value = (BS.replicate valueSize 0x42)
  return $ MetadataEntry subject value

bufferFittingMetadata :: Gen (Int, [MetadataEntry])
bufferFittingMetadata = do
  metadataCount <- choose (1, 10)
  metadataEntries <- vectorOf metadataCount genMetadataEntry
  let minimumBufferSize = sum (map (packedByteCount . Entry) metadataEntries)
  bufferSize <- choose (minimumBufferSize, minimumBufferSize + 200)
  return (bufferSize, metadataEntries)

bufferFillingMetadata :: Gen (Int, [MetadataEntry])
bufferFillingMetadata = do
  metadataEntries <- listOf1 genMetadataEntry
  let bufferSize = sum (map (packedByteCount . Entry) metadataEntries)
  return (bufferSize, metadataEntries)

bufferFittingAndOversizedMetadata :: Gen (Int, MetadataEntry, MetadataEntry)
bufferFittingAndOversizedMetadata = do
  smallData <- genMetadataEntry
  let smallDataSize = packedByteCount $ Entry smallData
  bufferLength <- choose (smallDataSize, smallDataSize + 200)
  largeData <- choose (bufferLength + 1, bufferLength + 100) >>= genSizedMetadataEntry
  return (bufferLength, smallData, largeData)

foldAppendMetadata :: BuilderMachine -> [MetadataEntry] -> IO (BuilderMachine, [MetadataItem])
foldAppendMetadata machine =
  foldM
    ( \(machine', acc) metadataData -> do
        (machine'', metadata') <- interpretCommand machine' (Append metadataData)
        return (machine'', acc ++ metadata')
    )
    (machine, [])

bufferBoundaryTests :: Spec
bufferBoundaryTests =
  describe "Buffer Boundary Tests" $ do
    prop "should not emit metadata when data exactly fills buffer, only after" $
      forAll bufferFillingMetadata $ \(bufferLength, entries) ->
        do
          machine <- mkMachine bufferLength
          (machine', emittedMetadata) <-
            foldAppendMetadata machine entries
          annotate "after appending exact fit data should not emit" $ length emittedMetadata `shouldBe` 0
          (_machine, metadata') <- interpretCommand machine' (Append $ head entries)
          case metadata' of
            [m] -> do
              annotate "number of entries should match" $ metadataItemEntriesCount m `shouldBe` length entries
              annotate "entries size should match" $ (sizeofByteArray $ metadataItemData m) `shouldBe` (sum $ map (packedByteCount . Entry) entries)
            l -> length l `shouldBe` 1

    prop "oversized append when buffer empty should emit one metadata" $
      forAll genMetadataEntry $ \metadata -> do
        let bufferLength = (packedByteCount metadata) - 1
        machine <- mkMachine bufferLength
        (_machine, metadataItems) <- interpretCommand machine (Append metadata)
        case metadataItems of
          [oversizedMetadata] -> do
            annotate "oversized metadata should have one entry" $ metadataItemEntriesCount oversizedMetadata `shouldBe` 1
            annotate "oversized metadata size should match input size" $ (sizeofByteArray $ metadataItemData oversizedMetadata) `shouldBe` (packedByteCount $ Entry metadata)
          l -> annotate "should emit one metadata" $ length l `shouldBe` 1

    prop "should emit oversized metadata and buffer when buffer is not empty" $
      forAll bufferFittingAndOversizedMetadata $
        \(bufferLength, smallData, largeData) -> do
          machine <- mkMachine bufferLength
          -- Add data that fits first
          (machine', metadata) <- interpretCommand machine (Append smallData)
          annotate "after adding small data should not emit" $
            length metadata `shouldBe` 0

          -- Now add oversized data
          (_machine, metadata') <- interpretCommand machine' (Append largeData)
          case metadata' of
            [firstMetadata, secondMetadata] -> do
              -- First metadata should contain the small data
              annotate "first metadata should have correct entries count" $ metadataItemEntriesCount firstMetadata `shouldBe` 1
              annotate "first metadata data should match small input size" $ (sizeofByteArray $ metadataItemData firstMetadata) `shouldBe` (packedByteCount $ Entry smallData)
              -- Second metadata should contain the large data
              annotate "second metadata should have correct entries count" $ metadataItemEntriesCount secondMetadata `shouldBe` 1
              annotate "second metadata should match large input size" $ (sizeofByteArray $ metadataItemData secondMetadata) `shouldBe` (packedByteCount $ Entry largeData)
            l -> annotate "after adding oversized data should emit two metadata" $ length l `shouldBe` 2

    prop "should handle multiple boundary crossings correctly" $ do
      let dataMetadata1Length = 26
          dataMetadata2Length = 14
      forAll (liftA2 (,) (genSizedMetadataEntry dataMetadata1Length) (genSizedMetadataEntry dataMetadata2Length)) $ \(dataMetadata1, dataMetadata2) -> do
        let bufferLength = 50
        machine <- mkMachine bufferLength
        -- Add data that will cause multiple boundary crossings
        (machine1, metadata1) <- interpretCommand machine (Append dataMetadata1)
        -- First add: 0 + 26 = 26, fits (total = 26)
        annotate "first addition should not emit" $ length metadata1 `shouldBe` 0

        (machine2, metadata2) <- interpretCommand machine1 (Append dataMetadata1)
        -- Second add: 26 + 26 = 52 > 50, so emit buffer with one entry (total = 26)
        -- Start new buffer with new data (total = 26)
        annotate "second addition" $ do
          case metadata2 of
            [metadata] -> do
              annotate "should emit one metadata with one entry" $ metadataItemEntriesCount metadata `shouldBe` 1
              annotate "metadata data size should match" $ (sizeofByteArray $ metadataItemData metadata) `shouldBe` dataMetadata1Length
            l -> annotate "should emit one metadata" $ length l `shouldBe` 1

        (machine3, metadata3) <- interpretCommand machine2 (Append dataMetadata2)
        -- Third add: 26 + 14 = 40, fits (total = 40)
        annotate "third addition should not emit" $ length metadata3 `shouldBe` 0

        (machine4, metadata4) <- interpretCommand machine3 (Append dataMetadata2)
        -- Fourth add: 40 + 14 = 54 > 50, so emit buffer with two entries (total = 40)
        -- Start new buffer with new data (total = 14)
        case metadata4 of
          [metadata] -> do
            annotate "fourth addition should emit one metadata with two entries" $ metadataItemEntriesCount metadata `shouldBe` 2
            annotate "metadata data size should match" $ (sizeofByteArray $ metadataItemData metadata) `shouldBe` (dataMetadata1Length + dataMetadata2Length)
          l -> annotate "fourth addition should emit one metadata" $ length l `shouldBe` 1

        (machine5, metadata5) <- interpretCommand machine4 (Append dataMetadata2)
        -- Fifth add: 14 + 14 = 28, fits (total = 28)
        annotate "fifth addition should not emit" $ length metadata5 `shouldBe` 0

        (machine6, metadata6) <- interpretCommand machine5 (Append dataMetadata2)
        -- Sixth add: 28 + 14 = 42, fits (total = 42)
        annotate "sixth addition should not emit" $ length metadata6 `shouldBe` 0

        (_digest, finalMetadata) <- interpretCommand machine6 Finalize
        case finalMetadata of
          Just metadata -> do
            annotate "final metadata should have three entries" $ metadataItemEntriesCount metadata `shouldBe` 3
            annotate "final metadata data size should match" $ (sizeofByteArray $ metadataItemData metadata) `shouldBe` dataMetadata2Length * 3
          Nothing -> assertFailure "Expected final metadata on finalization"

    describe "zero buffer length should always emit" $
      forM_ [0, 1, 4, 64, 255] $ \dataMetadataLength ->
        it ("should emit metadata immediately when buffer length is zero (dataLen =" ++ show dataMetadataLength ++ ")") $ do
          let bufferLength = 0
          let dataMetadata = MetadataEntry (BS.replicate dataMetadataLength 0x4B) BS.empty
          machine <- mkMachine bufferLength
          (_machine', metadata) <- interpretCommand machine (Append dataMetadata)
          case metadata of
            [m] -> do
              annotate "should emit one metadata with one entry" $ metadataItemEntriesCount m `shouldBe` 1
              annotate "metadata data size should match" $ (sizeofByteArray $ metadataItemData m) `shouldBe` dataMetadataLength + 8 + 4
            l -> annotate "should emit one metadata" $ length l `shouldBe` 1

finalizationTests :: Spec
finalizationTests =
  describe "Finalization Tests" $ do
    prop "should not emit metadata when finalizing empty buffer" $
      \(Positive bufferLength) -> do
        machine <- mkMachine bufferLength
        (digest, maybeMetadata) <- interpretCommand machine Finalize
        isNothing maybeMetadata `shouldBe` True
        -- Digest should still be computed (even if empty)
        annotate "digest should be present" $ case digest of Digest _ -> True `shouldBe` True

    prop "should emit fitting metadata only on finalize" $
      forAll bufferFittingMetadata $ \(bufferLength, metadataEntries) -> do
        machine <- mkMachine bufferLength
        (machine', metadata) <- foldAppendMetadata machine metadataEntries
        annotate "should not emit metadata before finalization" $ (length metadata) `shouldBe` 0

        (_digest, maybeMetadata) <- interpretCommand machine' Finalize
        case maybeMetadata of
          Just m -> do
            annotate "finalized metadata should have correct entries count" $ metadataItemEntriesCount m `shouldBe` length metadataEntries
            annotate "finalized metadata data should have correct size" $ (sizeofByteArray $ metadataItemData m) `shouldBe` (sum $ map (packedByteCount . Entry) metadataEntries)
          Nothing -> assertFailure "Expected metadata on finalization with data"
