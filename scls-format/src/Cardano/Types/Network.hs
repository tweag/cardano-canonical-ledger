{- |
Cardano bridge type that connects the network with cardano types.

All the network types are explicit and updates should be added to CIP first.
-}
module Cardano.Types.Network (
  NetworkId (..),
) where

import Data.Word (Word32)
import Foreign (castPtr)
import Foreign.Storable

-- | Identity of the network
data NetworkId
  = -- | Main cardano network
    Mainnet
  | -- | Test network
    Testnet
  deriving (Eq, Show)

-- TODO: define mempack or another class type for data serialisation

instance Storable NetworkId where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    n :: Word32 <- peek (castPtr ptr)
    return $! case n of
      0 -> Mainnet
      1 -> Testnet
      _ -> error "Unknown NetworkId"
  poke ptr Mainnet = poke (castPtr ptr) (0 :: Word32)
  poke ptr Testnet = poke (castPtr ptr) (1 :: Word32)
