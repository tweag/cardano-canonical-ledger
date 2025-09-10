{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.SCLS.Internal.Serializer.Reference.Impl (serialize)
import Cardano.Types.Network
import Cardano.Types.SlotNo
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.Text qualified as T
import Streaming.Prelude qualified as S
import System.Environment (getArgs)

-- TODO: introduce a context for file generator
-- we need to calculate running hash

-- Simple example of writing a file in SCLS format.
main :: IO ()
main = do
  [file_name] <- getArgs
  lazy_input <- BSL.getContents
  let chunks = chunksOf 1024 lazy_input
  serialize file_name Mainnet (SlotNo 0xDEADBEEF) (T.pack "raw/v0") (S.each chunks)

chunksOf :: Int64 -> BSL.ByteString -> [BS.ByteString]
chunksOf chunk_size bs = do
  case BSL.splitAt chunk_size bs of
    (chunk, rest)
      | BSL.null chunk -> []
      | otherwise -> BSL.toStrict chunk : chunksOf chunk_size rest
