{-# LANGUAGE NamedFieldPuns #-}
-- Example file for extracting raw contents
-- based on the file.
--
-- it will generate files "{namespace}.data"
-- for each namespace it sees
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- this is a tempororary file
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main where

import Cardano.SCLS.Internal.Frame
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Record.Hdr
import qualified Data.ByteString as BS
import Data.Char
import Data.Function (fix)
import qualified Data.Text as T
import System.Environment
import System.IO

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
            FrameView{frameRecordType}
              | frameRecordType == mkRecordType @Hdr -> pure ()
              | frameRecordType == mkRecordType @Chunk -> do
                  let Just decoded_frame :: Maybe (FrameView Chunk) = decodeFrame data_record
                  let fn = normalizeFile (T.unpack (chunkNamespace $ frameViewContent (decoded_frame)))
                  withFile fn AppendMode $ \h -> do
                    BS.hPut h (chunkData $ frameViewContent decoded_frame)
            _ -> pure ()
          go next_record
        Nothing -> putStrLn $ "No more records to read"
 where
  normalizeFile :: FilePath -> FilePath
  normalizeFile = map replaceBadChar
   where
    replaceBadChar c
      | isAlphaNum c = c
      | otherwise = '_'
