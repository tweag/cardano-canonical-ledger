module Main (main) where

import ChunksBuilderSpec (chunksBuilderTests)
import MetadataBuilderSpec (metadataBuilderTests)
import MultiNamespace qualified (tests)
import QuerySpec (queryTests)
import Roundtrip qualified (tests)

import Test.Hspec

main :: IO ()
main =
  hspec $ do
    Roundtrip.tests
    chunksBuilderTests
    metadataBuilderTests
    MultiNamespace.tests
    queryTests
