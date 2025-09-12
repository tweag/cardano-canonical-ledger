{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.SCLS.Internal.Reader (
  withNamespacedData,
) where

import Cardano.SCLS.Internal.Frame
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Serializer.MemPack
import Control.Monad (when)
import Control.Monad.Trans.Fail
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Function (fix)
import Data.MemPack (MemPack, unpackLeftOver)
import Data.Text (Text)
import Data.Typeable
import System.IO
import System.IO qualified as IO

import Streaming qualified as S
import Streaming.Prelude qualified as S

-- | Stream all data chunks for the given namespace.
withNamespacedData :: (MemPack u, Typeable u) => FilePath -> Text -> (S.Stream (S.Of u) IO () -> IO a) -> IO a
withNamespacedData filePath namespace f =
  IO.withFile filePath ReadMode \handle -> f (stream handle)
 where
  stream handle = do
    flip fix headerOffset \go record -> do
      next <- S.liftIO do
        fetchNextFrame handle record
      for_ next \next_record -> do
        dataRecord <- S.liftIO do
          fetchOffsetFrame handle next_record
        for_ (decodeFrame dataRecord) \chunkRecord -> do
          when (chunkNamespace (frameViewContent (chunkRecord)) == namespace) do
            flip fix (chunkData $ frameViewContent chunkRecord) \drain -> \case
              bs
                | BS.null bs -> pure ()
                | otherwise -> do
                    let (Entry userData, off) = errorFail $ unpackLeftOver bs
                        rest = BS.drop off bs
                    S.yield userData
                    drain rest
        go next_record
