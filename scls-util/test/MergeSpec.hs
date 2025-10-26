{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module MergeSpec (mergeCommandTests) where

import Cardano.SCLS.Internal.Reader (
  extractNamespaceList,
  extractRootHash,
 )
import Cardano.SCLS.Internal.Serializer.Dump (addChunks, defaultSerializationPlan)
import Cardano.SCLS.Internal.Serializer.MemPack (RawBytes (..))
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.Namespace qualified as Namespace
import Cardano.Types.Network (NetworkId (Mainnet))
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Common (generateTestFile, runSclsUtil)
import Control.Monad (forM)
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Expectations.Contrib (annotate)

generateSplitTestFiles :: FilePath -> IO ([(FilePath, Namespace)])
generateSplitTestFiles dir = do
  let testData =
        [ ("namespace1", [BS8.pack (show i) | i <- [1 :: Int .. 100]])
        , ("namespace2", [BS8.pack (show i) | i <- [1 :: Int .. 50]])
        , ("namespace3", [BS8.pack (show i) | i <- [1 :: Int .. 75]])
        ]

  forM testData \(ns, entries) -> do
    let fileName = dir </> Namespace.humanFileNameFor ns
        mkStream = S.yield (ns S.:> S.each (map RawBytes entries))

    Reference.serialize @RawBytes
      fileName
      Mainnet
      (SlotNo 1)
      (defaultSerializationPlan & addChunks mkStream)

    pure (fileName, ns)

generateOverlappingNsSplitTestFiles :: FilePath -> IO ([(FilePath, [Namespace])])
generateOverlappingNsSplitTestFiles dir = do
  let testData =
        [
          ( 1 :: Integer
          ,
            [ ("namespace1", [BS8.pack (show i) | i <- [1 :: Int .. 100]])
            , ("namespace2", [BS8.pack (show i) | i <- [1 :: Int .. 50]])
            ]
          )
        ,
          ( 2
          ,
            [ ("namespace2", [BS8.pack (show i) | i <- [51 :: Int .. 100]])
            , ("namespace3", [BS8.pack (show i) | i <- [1 :: Int .. 75]])
            ]
          )
        ,
          ( 3
          ,
            [ ("namespace3", [BS8.pack (show i) | i <- [76 :: Int .. 150]])
            ]
          )
        ]

  forM testData \(i, nsEntries) -> do
    let fileName = dir </> "file" ++ show i ++ ".scls"

    let stream =
          S.each nsEntries
            & S.map \(ns, entries) -> ns S.:> S.each (map RawBytes entries)

    Reference.serialize @RawBytes
      fileName
      Mainnet
      (SlotNo 1)
      (defaultSerializationPlan & addChunks stream)

    pure (fileName, map fst nsEntries)

mergeCommandTests :: Spec
mergeCommandTests = describe "merge command" do
  it "merges multiple files into one" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      testData <- generateSplitTestFiles dir

      let mergedFile = dir </> "merged.scls"

      (exitCode, _, _) <- runSclsUtil $ ["merge", mergedFile] ++ map fst testData

      exitCode `shouldBe` ExitSuccess

  it "handles files with overlapping namespaces" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      files <- generateOverlappingNsSplitTestFiles dir
      let mergedFile = dir </> "merged.scls"

      (exitCode, stdout, _) <- runSclsUtil $ ["merge", mergedFile] <> map fst files

      exitCode `shouldBe` ExitSuccess
      stdout `shouldContain` "3 unique namespace(s)"

  it "fails for non-existent source files" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let mergedFile = dir </> "merged.scls"
      (exitCode, _, _) <- runSclsUtil ["merge", mergedFile, "/nonexistent/file.scls"]

      exitCode `shouldBe` ExitFailure 1

  it "roundtrips" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (originalFile, namespaces) <- generateTestFile dir

      let splitDir = dir </> "split"
      (splitExitCode, _, _) <- runSclsUtil ["split", originalFile, splitDir]
      annotate "splits successfully" $ splitExitCode `shouldBe` ExitSuccess

      let mergedFile = dir </> "merged.scls"
      let splitFiles = [splitDir </> Namespace.humanFileNameFor ns | ns <- namespaces]
      (mergeExitCode, _, _) <- runSclsUtil $ ["merge", mergedFile] ++ splitFiles
      annotate "merges successfully" $ mergeExitCode `shouldBe` ExitSuccess

      originalNamespaces <- extractNamespaceList originalFile
      mergedNamespaces <- extractNamespaceList mergedFile
      annotate "namespace lists match" $ mergedNamespaces `shouldBe` originalNamespaces

      originalHash <- extractRootHash originalFile
      mergedHash <- extractRootHash mergedFile
      annotate "root hashes match" $ mergedHash `shouldBe` originalHash
