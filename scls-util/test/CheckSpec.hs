{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module CheckSpec (checkCommandTests) where

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

checkCommandTests :: Spec
checkCommandTests = describe "check command" do
  it "checkCommandTestsVerifies a valid SCLS file" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      -- arrange
      (fileName, _) <- generateTestFile dir
      (generateExitCode, _stdout, _) <- runSclsUtil ["debug", "output.scls", "--namespace", "utxo/v0:10"]
      generateExitCode `shouldBe` ExitSuccess
      -- act

      (checkExitCode, _stdout, _) <- runSclsUtil ["check", "output.scls"]

      -- assert
      checkExitCode `shouldBe` ExitSuccess

  it "fails for non-existent file" do
    (exitCode, _, _) <- runSclsUtil ["check", "/nonexistent/file.scls"]

    exitCode `shouldBe` ExitFailure 1
