{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module InfoSpec (infoCommandTests, listNsCommandTests) where

import Cardano.SCLS.Internal.Serializer.MemPack (RawBytes (..))
import Cardano.SCLS.Internal.Serializer.Reference.Dump (addChunks, defaultSerializationPlan)
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference
import Cardano.Types.Network (NetworkId (Mainnet))
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Common
import Control.Monad (forM_)
import Data.Function ((&))
import Data.Text qualified as T
import Streaming.Prelude qualified as S
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

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
          (defaultSerializationPlan & addChunks (S.each []))

      (exitCode, stdout, _) <- runSclsUtil ["list-ns", fileName]

      exitCode `shouldBe` ExitSuccess

      stdout `shouldContain` "No namespaces found"
