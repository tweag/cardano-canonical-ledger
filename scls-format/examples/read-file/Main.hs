-- Example of reading scls file without interpretation.
--
-- TODO: use some streaming library instead of the current approach?
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-} -- this is a tempororary file
module Main
  where

import System.IO
import Data.Function (fix)
import Cardano.SCLS.Internal.Frame
import System.Environment (getArgs)
import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Record.Manifest

main :: IO ()
main = do
    [file_name] <- getArgs
    withFile file_name ReadMode $ \handle -> do
        flip fix headerOffset $ \go record -> do
            putStrLn $ "Fetching next record after current offset #" ++ show (frameViewContent record)
            next <- fetchNextFrame handle record
            case next of
                Just next_record -> do
                    putStrLn $ "Has next record at #" ++ show (frameViewContent next_record) ++ " of type:" ++ show (frameRecordType next_record)
                    data_record <- fetchOffsetFrame handle next_record
                    case data_record of
                        FrameView { frameRecordType = 0 } -> do
                            let Just decoded_frame :: Maybe (FrameView Hdr) = decodeFrame data_record
                            putStrLn $ show (frameViewContent decoded_frame)
                        FrameView { frameRecordType = 0x10 } -> do
                            putStrLn $ "Decoding frame: " <> show (fmap Debug data_record)
                            let Just decoded_frame :: Maybe (FrameView Chunk) = decodeFrame data_record
                            putStrLn $ show (DebugChunk $ frameViewContent decoded_frame)
                        FrameView { frameRecordType = 0x01 } -> do
                            let Just decoded_frame :: Maybe (FrameView Manifest) = decodeFrame data_record
                            putStrLn $ show (frameViewContent decoded_frame)
                        _ ->
                            putStrLn $ "Unsupported frame: " <> show (fmap Debug data_record)
                    go next_record
                Nothing -> putStrLn $ "No more records to read"