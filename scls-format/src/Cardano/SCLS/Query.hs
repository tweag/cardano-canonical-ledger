{-# LANGUAGE LambdaCase #-}

module Cardano.SCLS.Query (queryEntry) where

import Cardano.SCLS.Internal.Reader (withNamespacedData)
import Cardano.SCLS.Internal.Serializer.Dump (HasKey (..))
import Cardano.Types.Namespace (Namespace)
import Data.MemPack (MemPack)
import Data.Typeable (Typeable)
import Streaming.Prelude qualified as S

queryEntry ::
  (HasKey a, MemPack a, Typeable a) =>
  FilePath ->
  Namespace ->
  Key a ->
  IO (Maybe a)
queryEntry dir ns key = do
  withNamespacedData dir ns $ \stream -> do
    value :: Maybe a <-
      S.head_
        $ S.dropWhile
          ( \entry ->
              let k = getKey entry
               in k < key
          )
          stream
    case value of
      Just entry | getKey entry == key -> pure $ Just entry
      Nothing -> pure Nothing
      Just _ -> pure Nothing
