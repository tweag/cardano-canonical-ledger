{-# LANGUAGE BlockArguments #-}

module Main (main) where

import InfoSpec
import MergeSpec
import SplitSpec
import Test.Hspec
import VerifySpec

main :: IO ()
main = hspec $ do
  describe "scls-util binary tests" do
    verifyCommandTests
    verifyNsCommandTests
    infoCommandTests
    listNsCommandTests
    splitCommandTests
    mergeCommandTests
