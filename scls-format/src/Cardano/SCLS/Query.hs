{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.SCLS.Query (queryEntry) where

import Cardano.SCLS.Internal.Frame (FrameView (frameViewContent), decodeFrame, fetchNextFrame, fetchOffsetFrame, headerOffset)
import Cardano.SCLS.Internal.Reader (decodeChunkEntries)
import Cardano.SCLS.Internal.Record.Chunk (Chunk (chunkData, chunkNamespace))
import Cardano.SCLS.Internal.Serializer.HasKey (HasKey (..))
import Cardano.Types.Namespace (Namespace)
import Control.Monad (mfilter)
import Control.Monad.Fix (fix)
import Data.Functor ((<&>))
import Data.MemPack (MemPack)
import Data.Typeable (Typeable)
import Streaming.Prelude qualified as S
import System.IO (IOMode (ReadMode), SeekMode (AbsoluteSeek), hSeek, withBinaryFile)

{- | Query for a specific entry by namespace and key.

This function searches for an entry with the given key within the specified
namespace and SCLS file. It leverages the assumption that entries are stored
in sorted order by key, allowing for early termination once a key greater than
or equal to the target is found.
-}
queryEntry ::
  (HasKey a, MemPack a, Typeable a) =>
  -- | The path for the SCLS file
  FilePath ->
  -- | The namespace to search within
  Namespace ->
  -- | The key of the entry to find
  Key a ->
  -- | Returns @Just entry@ if an entry with the exact key is found, @Nothing@ otherwise
  IO (Maybe a)
queryEntry filePath namespace key = do
  withBinaryFile
    filePath
    ReadMode
    query
 where
  query handle = do
    hSeek handle AbsoluteSeek 0
    flip fix (headerOffset, S.each []) \rec (offset, prev) -> do
      fetchNextFrame handle offset >>= \case
        Nothing ->
          -- No more records. If the entry exists, it must be on the previous record.
          findInStream prev
        Just nextOffset -> do
          dataRecord <- fetchOffsetFrame handle nextOffset
          case frameViewContent <$> decodeFrame dataRecord of
            Right chunk
              | chunkNamespace chunk == namespace -> do
                  let entries = decodeChunkEntries (chunkData chunk)
                  S.head_ entries >>= \case
                    Just entry
                      | (getKey entry == key) -> pure $ Just entry
                      | (getKey entry > key) ->
                          -- The key of the current block's first entry is bigger than the queried key.
                          -- If the entry exists, it must be on the previous record.
                          findInStream prev
                      | otherwise -> rec (nextOffset, entries)
                    Nothing -> rec (nextOffset, prev)
            _ -> rec (nextOffset, prev)

  findInStream stream = do
    S.head_ (S.dropWhile ((< key) . getKey) stream)
      <&> mfilter ((== key) . getKey)
