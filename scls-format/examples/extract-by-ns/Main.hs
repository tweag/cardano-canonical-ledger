-- Example file for extracting raw contents
-- based on the file.
--
-- it will generate files "{namespace}.data"
-- for each namespace it sees
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-} -- this is a tempororary file
module Main
  where

import Data.Function (fix)
import Data.Char
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Cardano.SCLS.Internal.Frame
import Cardano.SCLS.Internal.Block.Chunk
import System.IO
import System.Environment

main :: IO ()
main = do
    [input_file] <- getArgs
    withFile input_file ReadMode $ \handle -> do
        flip fix headerOffset $ \go block -> do
            next <- fetchNextFrame handle block
            case next of
                Just next_block -> do
                    data_block <- fetchOffsetFrame handle next_block
                    case data_block of
                        FrameView { frameBlockType = 0 } -> pure ()
                        FrameView { frameBlockType = 0x10 } -> do
                          let Just decoded_frame :: Maybe (FrameView Chunk) = decodeFrame data_block
                          let fn = normalizeFile (T.unpack (chunkNamespace $ frameViewContent (decoded_frame)))
                          withFile fn AppendMode $ \h -> do
                            BS.hPut h (chunkData $ frameViewContent decoded_frame)
                        _ -> pure ()
                    go next_block
                Nothing -> putStrLn $ "No more blocks to read"
  where
    normalizeFile :: FilePath -> FilePath
    normalizeFile = map replaceBadChar where
      replaceBadChar c
        | isAlphaNum c = c
        | otherwise = '_'

