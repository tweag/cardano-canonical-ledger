{-# LANGUAGE BlockArguments #-}

module Cardano.SCLS.Util.Tool (splitFile) where

import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Internal.Serializer.MemPack
import Cardano.SCLS.Internal.Serializer.Reference.Dump
import Cardano.SCLS.Util.Result
import Control.Exception (SomeException, catch)
import Data.Text qualified as T
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO

splitFile :: FilePath -> FilePath -> IO Result
splitFile sourceFile outputDir = do
  putStrLn $ "Splitting file: " ++ sourceFile
  putStrLn $ "Output directory: " ++ outputDir
  catch
    do
      createDirectoryIfMissing True outputDir
      hdr <- withHeader sourceFile pure
      namespaces <- extractNamespaceList sourceFile

      mapM_
        ( \ns -> do
            let outputFile = outputDir </> T.unpack ns ++ ".scls"
            putStrLn $ "  Creating " ++ outputFile ++ " for namespace " ++ T.unpack ns

            withBinaryFile outputFile WriteMode $ \handle -> do
              withNamespacedData @RawBytes sourceFile ns $ \stream -> do
                let dataStream = S.yield (ns S.:> stream)
                -- namespace-specific data should be sorted, so we can assume that and dump directly
                dumpToHandle handle hdr (DataStream dataStream)
        )
        namespaces

      putStrLn $ "Split complete. Generated these files:"
      mapM_ (putStrLn . ("  - " ++) . (outputDir </>) . (++ ".scls") . T.unpack) namespaces
      pure Ok
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError
