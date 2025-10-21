{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module VerifySpec (verifyCommandTests, verifyNsCommandTests) where

import Cardano.SCLS.Internal.Reader (
  extractNamespaceHash,
  extractRootHash,
 )
import Cardano.Types.Namespace qualified as Namespace
import Common
import Control.Monad (forM_)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

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
        (exitCode, stdout, _) <- runSclsUtil ["verify-ns", fileName, Namespace.asString ns]
        Just h <- extractNamespaceHash ns fileName

        exitCode `shouldBe` ExitSuccess

        stdout `shouldContain` (show h)

  it "fails for non-existent namespace" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (fileName, _) <- generateTestFile dir
      (exitCode, _, _) <- runSclsUtil ["verify-ns", fileName, "nonexistent"]

      exitCode `shouldBe` ExitFailure 65
