{-# LANGUAGE LambdaCase #-}

module Cardano.SCLS.Util.Query where

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
  withNamespacedData dir ns $ \stream ->
    S.fold_
      ( \case
          Just v -> \_ -> Just v
          Nothing -> \entry ->
            let k = getKey entry
             in if k == key then Just entry else Nothing
      )
      Nothing
      id
      stream
