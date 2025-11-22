{-# LANGUAGE TypeFamilies #-}

-- | Helper class that allows to extract keys from the stored entries.
module Cardano.SCLS.Internal.Serializer.HasKey (
  HasKey (..),
  sortByKey,
) where

import Data.Kind (Type)
import Data.List (sortOn)

-- | Class for types that have an associated key.
class (Ord (Key a)) => HasKey a where
  type Key a :: Type
  getKey :: a -> Key a

sortByKey :: (HasKey a) => [a] -> [a]
sortByKey = sortOn getKey
