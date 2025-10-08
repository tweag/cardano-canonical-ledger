{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.SCLS.Internal.Reader (
  extractNamespaceHash,
  extractRootHash,
 )
import Cardano.SCLS.Internal.Serializer.MemPack (RawBytes (..))
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference
import Cardano.Types.Network (NetworkId (Mainnet))
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text qualified as T
import Streaming.Prelude qualified as S
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "scls-util binary tests" do
    verifyCommandTests
    verifyNsCommandTests
    infoCommandTests
    listNsCommandTests

generateTestFile :: FilePath -> IO (FilePath, [Text])
generateTestFile dir = do
  let fileName = dir </> "test.scls"
      testData =
        [ ("namespace1", [BS8.pack (show i) | i <- [1 :: Int .. 100]])
        , ("namespace2", [BS8.pack (show i) | i <- [1 :: Int .. 50]])
        , ("namespace3", [BS8.pack (show i) | i <- [1 :: Int .. 75]])
        ]
      mkStream =
        S.each
          [ ns S.:> S.each (map RawBytes entries)
          | (ns, entries) <- testData
          ]

  _ <-
    Reference.serialize @RawBytes
      fileName
      Mainnet
      (SlotNo 1)
      mkStream

  return (fileName, map fst testData)

runSclsUtil :: [String] -> IO (ExitCode, String, String)
runSclsUtil args = do
  readProcessWithExitCode "cabal" (["run", "scls-util", "--"] ++ args) ""

verifyCommandTests :: Spec
verifyCommandTests = describe "verify command" do
  it "verifies a valid SCLS file" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (fileName, _) <- generateTestFile dir
      (exitCode, stdout, _) <- runSclsUtil ["verify", fileName]
      h <- extractRootHash fileName

      exitCode `shouldBe` ExitSuccess

      stdout `shouldContain` (show h)

  it "fails for non-existent file" do
    (exitCode, _, _) <- runSclsUtil ["verify", "/nonexistent/file.scls"]

    exitCode `shouldBe` ExitFailure 1

verifyNsCommandTests :: Spec
verifyNsCommandTests = describe "verify-ns command" do
  it "verifies all namespaces correctly" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (fileName, namespaces) <- generateTestFile dir

      forM_ namespaces \ns -> do
        (exitCode, stdout, _) <- runSclsUtil ["verify-ns", fileName, T.unpack ns]
        Just h <- extractNamespaceHash ns fileName

        exitCode `shouldBe` ExitSuccess

        stdout `shouldContain` (show h)

  it "fails for non-existent namespace" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (fileName, _) <- generateTestFile dir
      (exitCode, _, _) <- runSclsUtil ["verify-ns", fileName, "nonexistent"]

      exitCode `shouldBe` ExitFailure 65

infoCommandTests :: Spec
infoCommandTests = describe "info command" do
  it "displays file information" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (fileName, _) <- generateTestFile dir
      (exitCode, _, _) <- runSclsUtil ["info", fileName]

      exitCode `shouldBe` ExitSuccess

  it "lists all namespaces with their hashes" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (fileName, namespaces) <- generateTestFile dir
      (exitCode, stdout, _) <- runSclsUtil ["info", fileName]

      exitCode `shouldBe` ExitSuccess

      forM_ namespaces \ns -> do
        stdout `shouldContain` T.unpack ns

  it "fails for non-existent file" do
    (exitCode, stdout, stderr) <- runSclsUtil ["info", "/nonexistent/file.scls"]

    exitCode `shouldBe` ExitFailure 1

    (stdout <> stderr) `shouldContain` "Error"

listNsCommandTests :: Spec
listNsCommandTests = describe "list-ns command" do
  it "includes all expected namespaces" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (fileName, expectedNamespaces) <- generateTestFile dir

      (exitCode, stdout, _) <- runSclsUtil ["list-ns", fileName]

      exitCode `shouldBe` ExitSuccess

      forM_ expectedNamespaces \ns -> do
        stdout `shouldContain` T.unpack ns

  it "fails for non-existent file" do
    (exitCode, stdout, stderr) <- runSclsUtil ["list-ns", "/nonexistent/file.scls"]

    exitCode `shouldBe` ExitFailure 1

    (stdout <> stderr) `shouldContain` "Error"

  it "handles empty namespace list" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let fileName = dir </> "empty.scls"
      -- Create file with no namespaces
      _ <-
        Reference.serialize @RawBytes
          fileName
          Mainnet
          (SlotNo 1)
          (S.each [])

      (exitCode, stdout, _) <- runSclsUtil ["list-ns", fileName]

      exitCode `shouldBe` ExitSuccess

      stdout `shouldContain` "No namespaces found"
