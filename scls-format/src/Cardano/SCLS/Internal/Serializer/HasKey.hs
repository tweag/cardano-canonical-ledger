{-# LANGUAGE TypeFamilies #-}

-- | Helper class that allows to extract keys from the stored entries.
module Cardano.SCLS.Internal.Serializer.HasKey (
  HasKey (..),
) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.MemPack.Extra (RawBytes (..))

-- | Class for types that have an associated key.
class (Ord (Key a)) => HasKey a where
  type Key a :: Type
  getKey :: a -> Key a

instance HasKey RawBytes where
  type Key RawBytes = ByteString
  getKey (RawBytes bs) = bs
