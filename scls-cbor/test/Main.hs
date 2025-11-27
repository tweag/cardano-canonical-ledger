module Main where

import CanonicalSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    CanonicalSpec.tests
