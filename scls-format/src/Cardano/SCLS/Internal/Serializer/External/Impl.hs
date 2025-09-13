{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.SCLS.Internal.Serializer.External.Impl (
  serialize,
) where

import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Serializer.MemPack (Entry (..), RawBytes (..))
import Cardano.SCLS.Internal.Serializer.Reference.Dump
import Cardano.Types.Network
import Cardano.Types.SlotNo
import Control.Exception (bracket)
import Control.Monad.ST (runST)
import Data.ByteString qualified as B
import Data.Function ((&))
import Data.MemPack
import Data.PQueue.Prio.Min qualified as Q
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Tim qualified as Tim
import Data.Word (Word32)
import Streaming (MonadIO (liftIO), Of (..))
import Streaming qualified as S
import Streaming.Internal (Stream (..))
import Streaming.Prelude qualified as S
import System.ByteOrder
import System.Directory (listDirectory)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (IOMode (ReadMode, WriteMode), hClose, openBinaryFile, withBinaryFile)
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
  -- | Namespace for the data entries
  Text ->
  -- | Input stream of entries to serialize, can be unsorted
  Stream (Of a) IO () ->
  IO ()
serialize resultFilePath network slotNo namespace stream = do
  withTempDirectory (takeDirectory resultFilePath) "tmp.XXXXXX" \tmpDir -> do
    prepareExternalSort tmpDir stream
    let !hdr = mkHdr network slotNo
    withBinaryFile resultFilePath WriteMode \handle -> do
      kMerge tmpDir do dumpToHandle handle namespace hdr

{- | Simple version of the preparation step for external sorting.
It just splits an input into the sorted chunks of the fixed size.
-}
prepareExternalSort :: (Typeable a, Ord a, MemPack a) => FilePath -> Stream (Of a) IO () -> IO ()
prepareExternalSort tmpDir stream =
  -- TODO: split by data size, not by count
  -- TODO: use radix search first
  -- TODO: merge files in k-way merge way, ideally we want binomial tree merge
  stream
    & S.chunksOf 1024
    & S.mapped mkVector
    & S.zip (S.enumFrom 1)
    & S.mapM_ \(n, vec) -> do
      withBinaryFile (tmpDir </> "chunk" <.> show (n :: Int) <.> "bin") WriteMode \h -> do
        S.each vec
          & S.map (packByteString . Entry)
          & S.mapM_ (liftIO . B.hPut h)
 where
  mkVector = S.fold
    do \x e -> x <> Builder.singleton e
    do Builder.empty
    do
      \builder -> runST do
        mv <- Builder.build builder
        Tim.sort mv
        V.unsafeFreeze mv

kMerge :: FilePath -> (Stream (Of RawBytes) IO () -> IO a) -> IO a
kMerge tmpDir f = do
  bracket openAll closeAll \handles -> do
    pq <- mkPQ handles
    f (loop pq)
 where
  openAll = listDirectory tmpDir >>= mapM (\t -> openBinaryFile (tmpDir </> t) ReadMode)
  closeAll = mapM hClose
  mkPQ tt = do
    pairs <- mapM (\h -> (,) <$> (readNext h) <*> pure h) tt
    let pairs' = [(RawBytes bs, h) | (Just bs, h) <- pairs]
    return $ Q.fromList pairs'
  readNext h = do
    B.hGet h 4 >>= \case
      bs | B.null bs -> return Nothing
      bs -> do
        case fromBigEndian <$> unpack bs of
          Right ((fromIntegral -> (len :: Int)) :: Word32) -> do
            bs' <- B.hGet h (fromIntegral len)
            if B.length bs' < len
              then return Nothing
              else return (Just bs')
          Left{} -> return Nothing
  loop pq = case Q.minViewWithKey pq of
    Nothing -> return ()
    Just ((RawBytes bs, h), pq') -> do
      S.yield (RawBytes bs)
      liftIO (readNext h) >>= \case
        Nothing -> loop pq'
        Just bs' -> loop (Q.insert (RawBytes bs') h pq')
