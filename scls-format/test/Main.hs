module Main (main) where

import ChunksBuilderSpec (chunksBuilderTests)
import MultiNamespace qualified (tests)
import Roundtrip qualified (tests)

import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    Roundtrip.tests
    chunksBuilderTests
    MultiNamespace.tests
