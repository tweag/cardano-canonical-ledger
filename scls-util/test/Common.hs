{-# LANGUAGE OverloadedStrings #-}

module Common (generateTestFile, runSclsUtil) where

import Cardano.SCLS.Internal.Serializer.MemPack (RawBytes (..))
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.Network (NetworkId (Mainnet))
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Data.ByteString.Char8 qualified as BS8
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

generateTestFile :: FilePath -> IO (FilePath, [Namespace])
generateTestFile dir = do
  let fileName = dir </> "test.scls"
      testData =
        [ ("namespace1", [BS8.pack (show i) | i <- [1 :: Int .. 100]])
        , ("namespace2", [BS8.pack (show i) | i <- [1 :: Int .. 50]])
        , ("namespace3", [BS8.pack (show i) | i <- [1 :: Int .. 75]])
        ]
      mkStream =
        S.each
          [ ns S.:> S.each (map RawBytes entries)
          | (ns, entries) <- testData
          ]

  Reference.serialize @RawBytes
    fileName
    Mainnet
    (SlotNo 1)
    mkStream

  return (fileName, map fst testData)

runSclsUtil :: [String] -> IO (ExitCode, String, String)
runSclsUtil args = do
  readProcessWithExitCode "cabal" (["run", "scls-util", "--"] ++ args) ""
