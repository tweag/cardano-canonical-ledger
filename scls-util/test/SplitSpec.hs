{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module SplitSpec where

import Cardano.SCLS.Internal.Reader (
  extractNamespaceList,
  withLatestManifestFrame,
 )
import Cardano.SCLS.Internal.Record.Manifest
import Common
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Expectations.Contrib

splitCommandTests :: Spec
splitCommandTests = describe "split command" do
  it "splits a file by namespace" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (sourceFile, namespaces) <- generateTestFile dir
      let outputDir = dir </> "split"

      (exitCode, _, _) <- runSclsUtil ["split", sourceFile, outputDir]

      exitCode `shouldBe` ExitSuccess

      -- Verify each namespace was split into its own file
      forM_ namespaces \ns -> do
        let splitFile = outputDir </> T.unpack ns ++ ".scls"
        fileExists <- doesFileExist splitFile
        fileExists `shouldBe` True

        -- Verify the split file contains only one namespace
        splitNamespaces <- extractNamespaceList splitFile
        splitNamespaces `shouldBe` [ns]

  it "creates correct number of output files" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (sourceFile, namespaces) <- generateTestFile dir
      let outputDir = dir </> "split"

      (exitCode, _, _) <- runSclsUtil ["split", sourceFile, outputDir]

      annotate "exit code" $ exitCode `shouldBe` ExitSuccess

      withLatestManifestFrame
        ( \Manifest{nsInfo = originalNsInfo} -> do
            forM_ namespaces \ns -> do
              let splitFile = outputDir </> T.unpack ns ++ ".scls"
              withLatestManifestFrame
                ( \Manifest{..} -> do
                    annotate "only one namespace" $ Map.size nsInfo `shouldBe` 1
                    annotate "namespace info matches original" $ Map.lookup ns nsInfo `shouldBe` Map.lookup ns originalNsInfo
                )
                splitFile
        )
        sourceFile

  it "fails for non-existent file" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let outputDir = dir </> "split"
      (exitCode, _, _) <- runSclsUtil ["split", "/nonexistent/file.scls", outputDir]

      exitCode `shouldBe` ExitFailure 1
