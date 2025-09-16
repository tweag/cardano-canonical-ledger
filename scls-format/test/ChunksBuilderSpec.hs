{-# LANGUAGE OverloadedStrings #-}

module ChunksBuilderSpec (chunksBuilderTests) where

import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Record.Chunk (ChunkFormat (ChunkFormatRaw))
import Cardano.SCLS.Internal.Serializer.ChunksBuilder.InMemory
import Cardano.SCLS.Internal.Serializer.MemPack (RawBytes (..))
import Data.ByteString qualified as BS
import Data.Maybe (isJust, isNothing)
import Data.Primitive.ByteArray (sizeofByteArray)
import Test.HUnit

chunksBuilderTests :: Test
chunksBuilderTests =
  TestLabel "ChunksBuilder.InMemory Tests" $
    TestList
      [ bufferBoundaryTests
      , finalizationTests
      , edgeCaseTests
      ]

bufferBoundaryTests :: Test
bufferBoundaryTests =
  TestLabel "Buffer Boundary Tests" $
    TestList
      [ testFitsBuffer
      , testExactBoundary
      , testOversizedEmpty
      , testOversizedNonEmpty
      ]

finalizationTests :: Test
finalizationTests =
  TestLabel "Finalization Tests" $
    TestList
      [ testFinalizeEmpty
      , testFinalizeNonEmpty
      ]

edgeCaseTests :: Test
edgeCaseTests =
  TestLabel "Edge Case Tests" $ TestList [testMultipleBoundaryCrossings]

-- Test adding data that fits within buffer capacity
testFitsBuffer :: Test
testFitsBuffer = TestCase $ do
  let bufferLength = 100
  machine <- mkMachine bufferLength ChunkFormatRaw
  let smallData = RawBytes (BS.replicate 20 0x42)
  (machine', chunks) <- interpretCommand machine (Append smallData)
  assertEqual "Should not emit chunks when data fits" 0 (length chunks)

  let mediumData = RawBytes (BS.replicate 30 0x43)
  (_machine, chunks') <- interpretCommand machine' (Append mediumData)
  assertEqual "Should not emit chunks when accumulated data fits" 0 (length chunks')

-- Test adding records that exactly fill the buffer
testExactBoundary :: Test
testExactBoundary = TestCase $ do
  let bufferLength = 100
  machine <- mkMachine bufferLength ChunkFormatRaw
  let chunkDataLength = bufferLength - 4 -- 4 bytes for length prefix
  let exactFitData = RawBytes (BS.replicate chunkDataLength 0x44)
  (machine', chunks) <- interpretCommand machine (Append exactFitData)
  assertEqual "Should not emit chunks when data exactly fits" 0 (length chunks)

  -- Adding one more byte should trigger emission
  let oneByte = RawBytes (BS.replicate 1 0x45)
  (_machine, chunks') <- interpretCommand machine' (Append oneByte)
  assertEqual "Should emit exactly one chunk" 1 (length chunks')
  case chunks' of
    [chunk] -> do
      assertEqual "Chunk should have correct entries count" 1 (chunkItemEntriesCount chunk)
      assertBool "Chunk should use correct format" (case chunkItemFormat chunk of ChunkFormatRaw -> True; _ -> False)
      assertEqual "Chunk data should have correct size" (chunkDataLength + 4) (sizeofByteArray $ chunkItemData chunk)
    _ -> assertFailure "Expected exactly one chunk"

-- Test adding a record larger than buffer when buffer is empty
testOversizedEmpty :: Test
testOversizedEmpty = TestCase $ do
  let bufferLength = 50
  machine <- mkMachine bufferLength ChunkFormatRaw
  -- 50-3+4=51 bytes total with prefix, exceeding buffer
  let largeDataLength = bufferLength - 3
  let largeData = RawBytes (BS.replicate largeDataLength 0x46)
  (_machine, chunks) <- interpretCommand machine (Append largeData)
  -- In this case the implementation emits both the frozen buffer
  -- (which is empty in this case) and the oversized chunk
  -- TODO: consider changing this behavior to emit only the oversized chunk
  assertEqual "Should emit exactly two chunks for oversized data" 2 (length chunks)
  case chunks of
    [emptyChunk, oversizedChunk] -> do
      -- First chunk should be empty since buffer was empty
      assertEqual "Empty chunk should have zero entries" 0 (chunkItemEntriesCount emptyChunk)
      -- Second chunk should contain the oversized data
      assertEqual "Oversized chunk should have correct entries count" 1 (chunkItemEntriesCount oversizedChunk)
      assertEqual "Oversized chunk data should match input size" (largeDataLength + 4) (sizeofByteArray $ chunkItemData oversizedChunk)
    _ -> assertFailure "Expected exactly two chunks"

-- Test adding a record larger than buffer when buffer is not empty
testOversizedNonEmpty :: Test
testOversizedNonEmpty = TestCase $ do
  let bufferLength = 50
  machine <- mkMachine bufferLength ChunkFormatRaw
  -- Add data that fits first
  let smallDataLength = bufferLength - 40
  let smallData = RawBytes (BS.replicate smallDataLength 0x47)
  (machine', chunks) <- interpretCommand machine (Append smallData)
  assertEqual "Should not emit chunks for small data" 0 (length chunks)

  -- Now add oversized data
  -- 50-3+4=51 bytes total with prefix, exceeding buffer
  let largeDataLength = bufferLength - 3
  let largeData = RawBytes (BS.replicate largeDataLength 0x48)
  (_machine, chunks') <- interpretCommand machine' (Append largeData)
  assertEqual "Should emit exactly two chunks" 2 (length chunks')
  case chunks' of
    [firstChunk, secondChunk] -> do
      -- First chunk should contain the small data
      assertEqual "First chunk should have correct entries count" 1 (chunkItemEntriesCount firstChunk)
      assertEqual "First chunk data should match small input size" (smallDataLength + 4) (sizeofByteArray $ chunkItemData firstChunk)
      -- Second chunk should contain the large data
      assertEqual "Second chunk should have correct entries count" 1 (chunkItemEntriesCount secondChunk)
      assertEqual "Second chunk data should match large input size" (largeDataLength + 4) (sizeofByteArray $ chunkItemData secondChunk)
    _ -> assertFailure "Expected exactly two chunks"

-- Test finalization when buffer is empty
testFinalizeEmpty :: Test
testFinalizeEmpty = TestCase $ do
  let bufferLength = 10
  machine <- mkMachine bufferLength ChunkFormatRaw
  (digest, maybeChunk) <- interpretCommand machine Finalize
  assertBool "Should not emit chunk for empty buffer" (isNothing maybeChunk)
  -- Digest should still be computed (even if empty)
  assertBool "Digest should be present" (case digest of Digest _ -> True)

-- Test finalization when buffer contains data
testFinalizeNonEmpty :: Test
testFinalizeNonEmpty = TestCase $ do
  let bufferLength = 100
  machine <- mkMachine bufferLength ChunkFormatRaw
  let testDataLength = bufferLength - 30
  let testData = RawBytes (BS.replicate testDataLength 0x49)
  (machine', chunks) <- interpretCommand machine (Append testData)
  assertEqual "Should not emit chunks before finalization" 0 (length chunks)

  (_digest, maybeChunk) <- interpretCommand machine' Finalize
  case maybeChunk of
    Just chunk -> do
      assertEqual "Finalized chunk should have correct entries count" 1 (chunkItemEntriesCount chunk)
      assertBool "Finalized chunk should use correct format" (case chunkItemFormat chunk of ChunkFormatRaw -> True; _ -> False)
      assertEqual "Finalized chunk data should have correct size" (testDataLength + 4) (sizeofByteArray $ chunkItemData chunk)
    Nothing -> assertFailure "Expected chunk on finalization with data"

-- Test multiple boundary crossings
testMultipleBoundaryCrossings :: Test
testMultipleBoundaryCrossings = TestCase $ do
  let bufferLength = 50
  machine <- mkMachine bufferLength ChunkFormatRaw
  -- Add data that will cause multiple boundary crossings
  -- 26 bytes total with prefix
  let dataChunk1Length = 22
  let dataChunk1 = RawBytes (BS.replicate dataChunk1Length 0x4A)
  -- 14 bytes total with prefix
  let dataChunk2Length = 10
  let dataChunk2 = RawBytes (BS.replicate dataChunk2Length 0x4A)

  (machine1, chunks1) <- interpretCommand machine (Append dataChunk1)
  -- First add: 0 + 26 = 26, fits (total = 26)
  assertEqual "First addition should not emit" 0 (length chunks1)

  (machine2, chunks2) <- interpretCommand machine1 (Append dataChunk1)
  -- Second add: 26 + 26 = 52 > 50, so emit buffer with one entry (total = 26)
  -- Start new buffer with new data (total = 26)
  case chunks2 of
    [chunk] -> do
      assertEqual "Second addition should emit one chunk with one entry" 1 (chunkItemEntriesCount chunk)
      assertEqual "Chunk data size should match" (dataChunk1Length + 4) (sizeofByteArray $ chunkItemData chunk)
    l -> assertEqual "Second addition should emit one chunk" 1 (length l)

  (machine3, chunks3) <- interpretCommand machine2 (Append dataChunk2)
  -- Third add: 26 + 14 = 40, fits (total = 40)
  assertEqual "Third addition should not emit" 0 (length chunks3)

  (machine4, chunks4) <- interpretCommand machine3 (Append dataChunk2)
  -- Fourth add: 40 + 14 = 54 > 50, so emit buffer with two entries (total = 40)
  -- Start new buffer with new data (total = 14)
  case chunks4 of
    [chunk] -> do
      assertEqual "Fourth addition should emit one chunk with two entries" 2 (chunkItemEntriesCount chunk)
      assertEqual "Chunk data size should match" (dataChunk1Length + 4 + dataChunk2Length + 4) (sizeofByteArray $ chunkItemData chunk)
    l -> assertEqual "Fourth addition should emit one chunk" 1 (length l)

  (machine5, chunks5) <- interpretCommand machine4 (Append dataChunk2)
  -- Fifth add: 14 + 14 = 28, fits (total = 28)
  assertEqual "Fifth addition should not emit" 0 (length chunks5)

  (machine6, chunks6) <- interpretCommand machine5 (Append dataChunk2)
  -- Sixth add: 28 + 14 = 42, fits (total = 42)
  assertEqual "Sixth addition should not emit" 0 (length chunks6)

  (_digest, finalChunk) <- interpretCommand machine6 Finalize
  case finalChunk of
    Just chunk -> do
      assertEqual "Final chunk should have three entries" 3 (chunkItemEntriesCount chunk)
      assertEqual "Final chunk data size should match" ((dataChunk2Length + 4) * 3) (sizeofByteArray $ chunkItemData chunk)
    Nothing -> assertFailure "Expected final chunk on finalization"
