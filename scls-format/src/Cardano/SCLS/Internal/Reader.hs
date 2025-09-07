{-# LANGUAGE LambdaCase #-}
module Cardano.SCLS.Internal.Reader
  ( withNamespacedData
  ) where

import System.IO
import Data.Function (fix)
import qualified Data.ByteString as BS
import Cardano.SCLS.Internal.Frame
import Cardano.SCLS.Internal.Record.Chunk
import Data.Text (Text)
import System.IO qualified as IO
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Typeable
import Data.MemPack (unpackLeftOver, MemPack)
import Control.Monad.Trans.Fail
import Cardano.SCLS.Internal.Serializer.MemPack

import Streaming qualified as S
import Streaming.Prelude qualified as S

-- | Stream all data chunks for the given namespace.
withNamespacedData :: (MemPack u, Typeable u) => FilePath -> Text -> (S.Stream (S.Of u) IO () -> IO a) -> IO a
withNamespacedData filePath namespace f =
    IO.withFile filePath ReadMode $ \handle -> f (stream handle)
  where
    stream handle = do
        flip fix headerOffset $ \go record -> do
            next <- S.liftIO $ fetchNextFrame handle record
            for_ next $ \next_record -> do
                dataRecord <- S.liftIO $ fetchOffsetFrame handle next_record
                for_ (decodeFrame dataRecord) $ \chunkRecord -> do
                    when (chunkNamespace (frameViewContent (chunkRecord)) == namespace) $ do
                        flip fix (chunkData $ frameViewContent chunkRecord) $ \drain -> \case
                          bs | BS.null bs -> pure ()
                             | otherwise -> do
                                let (Entry userData, off) = errorFail $ unpackLeftOver bs
                                    rest = BS.drop off bs
                                S.yield userData
                                drain rest
                go next_record