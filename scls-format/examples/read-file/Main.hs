-- Example of reading scls file without interpretation.
--
-- TODO: use some streaming library instead of the current approach?
{-# LANGUAGE ScopedTypeVariables #-}
module Main
  where

import System.IO
import Data.Function (fix)
import Data.Foldable (for_)
import Cardano.SCLS.Internal.Frame
import System.Environment (getArgs)
import Cardano.SCLS.Internal.Block.Hdr
import Cardano.SCLS.Internal.Block.Chunk
import Cardano.SCLS.Internal.Block.Manifest

main :: IO ()
main = do
    [file_name] <- getArgs
    withFile file_name ReadMode $ \handle -> do
        flip fix headerOffset $ \go block -> do
            putStrLn $ "Fetching next block after current offset #" ++ show (frameViewContent block)
            next <- fetchNextFrame handle block
            case next of
                Just next_block -> do
                    putStrLn $ "Has next block at #" ++ show (frameViewContent next_block) ++ " of type:" ++ show (frameBlockType next_block)
                    data_block <- fetchOffsetFrame handle next_block
                    case data_block of
                        FrameView { frameBlockType = 0 } -> do
                            let Just decoded_frame :: Maybe (FrameView Hdr) = decodeFrame data_block
                            putStrLn $ show (frameViewContent decoded_frame)
                        FrameView { frameBlockType = 0x10 } -> do
                            putStrLn $ "Decoding frame: " <> show (fmap Debug data_block)
                            let Just decoded_frame :: Maybe (FrameView Chunk) = decodeFrame data_block
                            putStrLn $ show (DebugChunk $ frameViewContent decoded_frame)
                        FrameView { frameBlockType = 0x01 } -> do
                            let Just decoded_frame :: Maybe (FrameView Manifest) = decodeFrame data_block
                            putStrLn $ show (frameViewContent decoded_frame)
                        _ ->
                            putStrLn $ "Unsupported frame: " <> show (fmap Debug data_block)
                    go next_block
                Nothing -> putStrLn $ "No more blocks to read"