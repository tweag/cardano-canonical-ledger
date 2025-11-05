{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module CheckSpec (checkCommandTests) where

import Common
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

checkCommandTests :: Spec
checkCommandTests = describe "check command" do
  it "checkCommandTestsVerifies a valid SCLS file" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      -- arrange
      (generateExitCode, _stdout, _) <- runSclsUtil ["debug", dir </> "output.scls", "--namespace", "utxo/v0:10"]
      generateExitCode `shouldBe` ExitSuccess
      -- act

      (checkExitCode, _stdout, _) <- runSclsUtil ["check", dir </> "output.scls"]

      -- assert
      checkExitCode `shouldBe` ExitSuccess

  it "fails for non-existent file" do
    (exitCode, _, _) <- runSclsUtil ["check", "/nonexistent/file.scls"]

    exitCode `shouldBe` ExitFailure 1
