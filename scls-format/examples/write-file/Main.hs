{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.SCLS.Internal.Frame
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Record.Manifest
import Cardano.Types.Network
import Cardano.Types.SlotNo
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.List (genericLength)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Environment (getArgs)
import System.IO

-- TODO: introduce a context for file generator
-- we need to calculate running hash

-- Simple example of writing a file in SCLS format.
main :: IO ()
main = do
  [file_name] <- getArgs
  withFile file_name WriteMode $ \handle -> do
    _ <- hWriteFrame handle $ mkHdr Mainnet (SlotNo 0xDEADBEEF)
    lazy_input <- BSL.getContents
    for_ (zip [0 ..] (BSL.toChunks lazy_input)) $ \(idx, sbs) -> do
      hWriteFrame handle $
        mkChunk idx ChunkFormatRaw (T.pack "raw/v0") sbs 1
    let rootHash = BS.replicate 28 16
        nsRoots = Map.fromList [(T.pack "raw/v0", rootHash)]
        prevManifestOffset = 0
        summary =
          ManifestSummary
            { createdAt = T.pack "2025-01-01T00:00:00Z"
            , tool = T.pack "example-write-1.0"
            , comment = Just (T.pack "debug tool")
            }
        totalEntries = genericLength (BSL.toChunks lazy_input)
        totalChunks = genericLength (BSL.toChunks lazy_input)
    _ <- hWriteFrame handle Manifest{..}
    pure ()
