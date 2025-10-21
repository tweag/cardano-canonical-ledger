{-# LANGUAGE TypeFamilies #-}

-- | Helper class that allows to extract keys from the stored entries.
module Cardano.SCLS.Internal.Serializer.HasKey (
  HasKey (..),
) where

import Data.Kind (Type)

-- | Class for types that have an associated key.
class (Eq (Key a)) => HasKey a where
  type Key a :: Type
  getKey :: a -> Key a
