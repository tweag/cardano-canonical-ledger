{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.SCLS.Internal.Serializer.External.Impl (
  serialize,
) where

import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Record.Metadata
import Cardano.SCLS.Internal.Serializer.MemPack (Entry (..), RawBytes (..))
import Cardano.SCLS.Internal.Serializer.Reference.Dump
import Cardano.Types.Network
import Cardano.Types.SlotNo

import Control.Exception (onException)
import Control.Monad.ST (runST)
import Data.ByteString qualified as B
import Data.Function (fix, (&))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)

-- import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.MemPack

import Data.PQueue.Prio.Min qualified as Q
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Tim qualified as Tim

import Data.Word (Word32)
import Streaming (MonadIO (liftIO), Of (..))
import Streaming qualified as S
import Streaming.Internal (Stream (..))
import Streaming.Prelude qualified as S

import System.ByteOrder
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile, renameFile)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (Handle, IOMode (ReadMode, WriteMode), hClose, openBinaryFile, withBinaryFile)
import System.IO.Temp (withTempDirectory)
import VectorBuilder.Builder qualified as Builder
import VectorBuilder.MVector qualified as Builder

serialize ::
  (MemPack a, Ord a, Typeable a) =>
  -- | path to resulting file
  FilePath ->
  -- | Network identifier
  NetworkId ->
  -- | Slot of the current transaction
  SlotNo ->
  -- | Input stream of entries to serialize, can be unsorted
  S.Stream (S.Of (InputChunk a)) IO () ->
  {- | Input stream of metadata to serialize
  TODO: this currently assumes data is sorted
  -}
  S.Stream (S.Of Metadata) IO () ->
  IO ()
serialize resultFilePath network slotNo inputStream metadataStream = do
  let !hdr = mkHdr network slotNo
  withTempDirectory (takeDirectory resultFilePath) "tmp.XXXXXX" \tmpDir -> do
    prepareExternalSortNamespaced tmpDir inputStream
    handles <- newIORef []
    onException
      do
        withBinaryFile resultFilePath WriteMode \handle -> do
          dumpToHandle handle hdr $
            newDumpConfig
              & withChunks (sourceNs handles tmpDir)
              & withMetadata metadataStream
      do traverse hClose =<< readIORef handles

{- | Accepts an unordered stream of entries, and prepares a structure of
the sorted files in the filesystem.

The structure is the following:

@
root/<namespace>/chunk.n.bin
@

`n` is a number that marks the level in the tree. We start with `0`,
where there is a 1024 elements, and then update the number, if there
is an existing file already we merge them and increase the number and
so on, until we have placed a file.

1024 elements is not a great strategy as it does not take into account
the size of the entries, but it can be changed without modifying the interface.
-}
prepareExternalSortNamespaced ::
  (Typeable a, Ord a, MemPack a) =>
  FilePath ->
  S.Stream (S.Of (InputChunk a)) IO () ->
  IO ()
prepareExternalSortNamespaced tmpDir = storeChunks . mergeChunks
 where
  storeChunks = S.mapM_ \(namespace, vec) -> do
    let dir = tmpDir </> T.unpack namespace
    let mkFileName i = tmpDir </> T.unpack namespace </> "chunk" <.> show (i :: Int) <.> "bin"
    liftIO $ createDirectoryIfMissing True dir
    withBinaryFile (mkFileName 0) WriteMode \h ->
      S.each vec
        & S.map (packByteString . Entry)
        & S.mapM_ (liftIO . B.hPut h)
    flip fix 0 \go n -> do
      exists <- doesFileExist (mkFileName (n + 1))
      if exists
        then do
          merge2 (mkFileName n) (mkFileName (n + 1))
          go (n + 1)
        else renameFile (mkFileName n) (mkFileName (n + 1))

{- | Consume streams generating chunks of data by 1024 entries in size
the input may be unordered and we can have a namespaces to appear
multiple times in the stream
-}
mergeChunks ::
  (Ord a) =>
  S.Stream (S.Of (InputChunk a)) IO () ->
  S.Stream (S.Of (Text, V.Vector a)) IO ()
mergeChunks = loop Map.empty
 where
  chunkSize = 1024
  loop s (Step ((ns :> vecStream) :> rest)) = do
    let i = fromMaybe Builder.empty $ Map.lookup ns s
    Effect do
      (v :> r) <-
        vecStream
          & S.splitAt (chunkSize - Builder.size i)
          & S.toList
      case v of
        -- Nothing in the current chunk, just continue
        [] -> return $ loop s rest
        _ ->
          let i' = i <> Builder.foldable v
           in if Builder.size i' < chunkSize -- we were no able to fill the chunk, so r is empty
                then return $ loop (Map.insert ns i' s) (rest)
                else do
                  let v' = runST do
                        mv <- Builder.build i'
                        Tim.sort mv
                        V.unsafeFreeze mv
                  return $ S.yield (ns, v') >> loop (Map.delete ns s) (Step ((ns :> r) :> rest))
  loop s (Effect e) = Effect (e >>= \s' -> return (loop s s'))
  loop s (Return _) = do
    S.each (Map.toList s)
      & S.map \(ns, builder) ->
        let v = runST do
              mv <- Builder.build builder
              Tim.sort mv
              V.unsafeFreeze mv
         in (ns, v)

merge2 :: FilePath -> FilePath -> IO ()
merge2 f1 f2 = do
  let outFile = f2 ++ ".merged"
  withBinaryFile f1 ReadMode \h1 ->
    withBinaryFile f2 ReadMode \h2 -> do
      withBinaryFile outFile WriteMode \hout -> do
        doMerge h1 h2 hout
  removeFile f1
  renameFile outFile f2
  pure ()
 where
  doMerge h1 h2 hout = do
    a <- readNext h1
    b <- readNext h2
    go a b
   where
    go a b = case (a, b) of
      (Nothing, Nothing) -> pure ()
      (Just bs1, Nothing) -> do
        B.hPut hout (packByteString $ Entry $ RawBytes bs1)
        copyAll h1
      (Nothing, Just bs2) -> do
        B.hPut hout (packByteString $ Entry $ RawBytes bs2)
        copyAll h2
      (Just bs1, Just bs2) ->
        if bs1 <= bs2
          then do
            B.hPut hout (packByteString $ Entry $ RawBytes bs1)
            bs1' <- readNext h1
            go bs1' b
          else do
            B.hPut hout (packByteString $ Entry $ RawBytes bs2)
            bs2' <- readNext h2
            go a bs2'

    -- Efficiently copy the rest of the file without parsing entries
    copyAll hin = do
      let chunkSize = 32768 -- 32 KiB, adjust as needed
      let loop = do
            chunk <- B.hGetSome hin chunkSize
            if B.null chunk
              then pure ()
              else B.hPut hout chunk >> loop
      loop

-- | Create a stream from the list of namespaces.
sourceNs :: IORef [Handle] -> FilePath -> DataStream RawBytes
sourceNs handles baseDir = DataStream do
  ns <- liftIO $ listDirectory baseDir
  S.each ns & S.map (\n -> (T.pack n :> kMergeNs handles (baseDir </> n)))

{- | K-merge files from the multiple namespaces.

Keeps track of handles being opened.
-}
kMergeNs :: IORef [Handle] -> FilePath -> Stream (Of RawBytes) IO ()
kMergeNs refs dir = do
  files <- liftIO $ listDirectory dir
  handles <- liftIO $ mapM (\f -> openBinaryFile (dir </> f) ReadMode >>= \x -> modifyIORef' refs (x :) >> pure x) files
  pq <- liftIO $ mkPQ handles
  inner pq
 where
  inner pq = case Q.minViewWithKey pq of
    Nothing -> return ()
    Just ((bs, h), pq') -> do
      S.yield (RawBytes bs)
      liftIO (readNext h) >>= \case
        Nothing -> do
          liftIO $ hClose h >> modifyIORef' refs (filter (/= h))
          inner pq'
        Just bs' -> inner (Q.insert bs' h pq')

  mkPQ tt = do
    pairs <- mapM (\h -> (,) <$> (readNext h) <*> pure h) tt
    let pairs' = [(bs, h) | (Just bs, h) <- pairs]
    return $ Q.fromList pairs'

readNext :: Handle -> IO (Maybe B.ByteString)
readNext h = do
  B.hGet h 4 >>= \case
    bs | B.null bs -> return Nothing
    bs -> case fromBigEndian <$> unpack bs of
      Right ((fromIntegral -> (len :: Int)) :: Word32) -> do
        bs' <- B.hGet h (fromIntegral len)
        if B.length bs' < len
          then return Nothing
          else return (Just bs')
      Left{} -> return Nothing
