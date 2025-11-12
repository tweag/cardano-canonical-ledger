{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.MemPack.IsKey (IsKey (..)) where

import Data.MemPack
import Data.MemPack.Buffer
import Data.MemPack.Extra
import Data.Proxy
import GHC.TypeLits (KnownNat, natVal)

class (Ord a) => IsKey a where
  keySize :: Int
  packKeyM :: a -> Pack b ()
  unpackKeyM :: forall b. (Buffer b) => Unpack b a

instance (KnownNat n) => IsKey (ByteStringSized n) where
  keySize = fromInteger (natVal (Proxy :: Proxy n))
  packKeyM = packM
  unpackKeyM = unpackM
