{-# LANGUAGE OverloadedStrings #-}

module Main where

import NamespacedEncodingSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  NamespacedEncodingSpec.spec
