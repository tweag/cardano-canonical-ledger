{-# LANGUAGE LambdaCase #-}

module Cardano.SCLS.Query (queryEntry) where

import Cardano.SCLS.Internal.Reader (withNamespacedData)
import Cardano.SCLS.Internal.Serializer.Dump (HasKey (..))
import Cardano.Types.Namespace (Namespace)
import Data.MemPack (MemPack)
import Data.Typeable (Typeable)
import Streaming.Prelude qualified as S

-- | Query for a specific entry by namespace and key.
--
-- This function searches for an entry with the given key within the specified
-- namespace in the SCLS data directory. It leverages the assumption that entries
-- are stored in sorted order by key, allowing for early termination once a key
-- greater than or equal to the target is found.
queryEntry ::
  (HasKey a, MemPack a, Typeable a) =>
  -- | The directory path containing the SCLS data files
  FilePath ->  
  -- | The namespace to search within
  Namespace ->
  -- | The key of the entry to find
  Key a ->
  -- | Returns @Just entry@ if an entry with the exact key is found, @Nothing@ otherwise
  IO (Maybe a)
queryEntry dir ns key = do
  withNamespacedData dir ns $ \stream -> do
    value :: Maybe a <-
      S.head_ $
        S.dropWhile
          ( \entry ->
              let k = getKey entry
               in k < key
          )
          stream
    case value of
      Just entry | getKey entry == key -> pure $ Just entry
      Nothing -> pure Nothing
      Just _ -> pure Nothing
