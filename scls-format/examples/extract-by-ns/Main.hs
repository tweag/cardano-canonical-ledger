-- Example file for extracting raw contents
-- based on the file.
--
-- it will generate files "{namespace}.data"
-- for each namespace it sees
{-# LANGUAGE ScopedTypeVariables #-}
module Main
  where

import Data.Function (fix)
import Data.Char
import qualified Data.Text as T
import System.FilePath
import qualified Data.ByteString as BS
import Cardano.SCLS.Internal.Frame
import Cardano.SCLS.Internal.Record.Chunk
import System.IO
import System.Environment

main :: IO ()
main = do
    [input_file] <- getArgs
    withFile input_file ReadMode $ \handle -> do
        flip fix headerOffset $ \go record -> do
            next <- fetchNextFrame handle record
            case next of
                Just next_record -> do
                    data_record <- fetchOffsetFrame handle next_record
                    case data_record of
                        FrameView { frameRecordType = 0 } -> pure ()
                        FrameView { frameRecordType = 0x10 } -> do
                          let Just decoded_frame :: Maybe (FrameView Chunk) = decodeFrame data_record
                          let fn = normalizeFile (T.unpack (chunkNamespace $ frameViewContent (decoded_frame)))
                          withFile fn AppendMode $ \h -> do
                            BS.hPut h (chunkData $ frameViewContent decoded_frame)
                        _ -> pure ()
                    go next_record
                Nothing -> putStrLn $ "No more records to read"
  where
    normalizeFile :: FilePath -> FilePath
    normalizeFile = map replaceBadChar where
      replaceBadChar c
        | isAlphaNum c = c
        | otherwise = '_'

