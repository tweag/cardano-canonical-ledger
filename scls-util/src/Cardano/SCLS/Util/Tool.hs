{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Util.Tool (splitFile, mergeFiles, extract, ExtractOptions (..)) where

import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Internal.Record.Hdr (Hdr (..))
import Cardano.SCLS.Internal.Serializer.Dump
import Cardano.SCLS.Internal.Serializer.Dump.Plan (addChunks, defaultSerializationPlan, mkSortedSerializationPlan)
import Cardano.SCLS.Internal.Serializer.External.Impl (serialize)
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.Namespace qualified as Namespace
import Cardano.Types.Network (NetworkId (Mainnet))
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Control.Exception (SomeException, catch)
import Control.Monad (foldM, forM_)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.MemPack.Extra
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
            let outputFile = outputDir </> Namespace.humanFileNameFor ns
            putStrLn $ "  Creating " ++ outputFile ++ " for namespace " ++ Namespace.asString ns

            withBinaryFile outputFile WriteMode $ \handle -> do
              withNamespacedData @RawBytes sourceFile ns $ \stream -> do
                let dataStream = S.yield (ns S.:> stream)
                -- namespace-specific data should be sorted, so we can assume that and dump directly
                dumpToHandle handle hdr (mkSortedSerializationPlan (defaultSerializationPlan & addChunks dataStream) id)
        )
        namespaces

      putStrLn $ "Split complete. Generated these files:"
      mapM_ (putStrLn . ("  - " ++) . (outputDir </>) . Namespace.humanFileNameFor) namespaces
      pure Ok
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError

mergeFiles :: FilePath -> [FilePath] -> IO Result
mergeFiles _ [] = do
  putStrLn "No source files provided for merging"
  pure OtherError
mergeFiles outputFile sourceFiles = do
  putStrLn $ "Merging " ++ show (length sourceFiles) ++ " file(s) into: " ++ outputFile
  catch
    do
      nsToFiles <- collectNamespaceFiles sourceFiles

      putStrLn $ "Found " ++ show (Map.size nsToFiles) ++ " unique namespace(s)"

      withNamespaceHandles nsToFiles $ \nsHandles -> do
        let stream =
              S.each nsHandles
                & S.mapM_ \(ns, handles) -> do
                  S.each handles
                    & S.map
                      ( \handle ->
                          (ns S.:> namespacedData @RawBytes handle ns)
                      )

        serialize
          outputFile
          Mainnet
          (SlotNo 1)
          (defaultSerializationPlan & addChunks stream)

      putStrLn "Merge complete"
      pure Ok
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError
 where
  collectNamespaceFiles :: [FilePath] -> IO (Map.Map Namespace [FilePath])
  collectNamespaceFiles files = do
    foldM
      ( \acc f -> do
          foldl'
            ( \m ns -> do
                let l = maybe [f] ((:) f) (Map.lookup ns m)
                Map.insert ns l m
            )
            acc
            <$> extractNamespaceList f
      )
      mempty
      files
  withNamespaceHandles :: Map Namespace [FilePath] -> ([(Namespace, [Handle])] -> IO a) -> IO a
  withNamespaceHandles nsToFiles action = do
    nsHandles <-
      Map.foldrWithKey
        ( \ns files acc -> do
            handles <- mapM (\file -> openFile file ReadMode) files
            (:) (ns, handles) <$> acc
        )
        (pure [])
        nsToFiles

    result <- action nsHandles

    forM_
      nsHandles
      (mapM_ hClose . snd)

    pure result

data ExtractOptions = ExtractOptions
  { extractNamespaces :: Maybe [Namespace]
  }

extract :: FilePath -> FilePath -> ExtractOptions -> IO Result
extract sourceFile outputFile ExtractOptions{..} = do
  putStrLn $ "Extracting from file: " ++ sourceFile
  putStrLn $ "Output file: " ++ outputFile
  catch
    do
      Hdr{..} <- withHeader sourceFile pure

      withBinaryFile sourceFile ReadMode \handle -> do
        let chunks =
              case extractNamespaces of
                Nothing -> S.each []
                Just nsList ->
                  S.each nsList
                    & S.map
                      (\ns -> (ns S.:> namespacedData @RawBytes handle ns))

        serialize
          outputFile
          networkId
          slotNo
          (defaultSerializationPlan & addChunks chunks)

      pure Ok
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError
