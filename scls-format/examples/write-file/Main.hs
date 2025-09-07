{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.SCLS.Internal.Serializer.Reference (serialize)
import Cardano.Types.Network
import Cardano.Types.SlotNo
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import qualified Data.Text as T
import System.Environment (getArgs)

-- TODO: introduce a context for file generator
-- we need to calculate running hash

-- Simple example of writing a file in SCLS format.
main :: IO ()
main = do
  [file_name] <- getArgs
  lazy_input <- BSL.getContents
  let chunks = chunksOf 1024 lazy_input
  serialize file_name Mainnet (SlotNo 0xDEADBEEF) (T.pack "raw/v0") chunks

chunksOf :: Int64 -> BSL.ByteString -> [BS.ByteString]
chunksOf chunk_size bs = do
  case BSL.splitAt chunk_size bs of
    (chunk, rest)
      | BSL.null chunk -> []
      | otherwise -> BSL.toStrict chunk : chunksOf chunk_size rest
