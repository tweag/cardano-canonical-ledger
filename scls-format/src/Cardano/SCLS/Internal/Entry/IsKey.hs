{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.SCLS.Internal.Entry.IsKey (
  IsKey (..),
) where

import Cardano.SCLS.Internal.Serializer.MemPack (ByteStringSized (..))
import Data.Data (Typeable)
import Data.MemPack
import Data.MemPack.Buffer
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, natVal)

class (Ord a, Typeable a) => IsKey a where
  keySize :: Int
  packKeyM :: a -> Pack b ()
  unpackKeyM :: forall b s. (Buffer b) => Unpack s b a

instance (KnownNat n) => IsKey (ByteStringSized n) where
  keySize = fromInteger (natVal (Proxy :: Proxy n))
  packKeyM = packM
  unpackKeyM = unpackM
