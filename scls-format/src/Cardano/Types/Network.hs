{- |
Cardano bridge type that connects the network with cardano types.

All the network types are explicit and updates should be added to CIP first.
-}
module Cardano.Types.Network (
  NetworkId (..),
) where

import Data.MemPack (MemPack (..), packTagM, unpackTagM)
import Data.Word (Word8)
import Foreign (castPtr)
import Foreign.Storable

-- | Identity of the network
data NetworkId
  = -- | Main cardano network
    Mainnet
  | -- | Test network
    Testnet
  deriving (Eq, Show)

instance Storable NetworkId where
  sizeOf _ = 1
  alignment _ = 1
  peek ptr = do
    n :: Word8 <- peek (castPtr ptr)
    return $! case n of
      0 -> Mainnet
      1 -> Testnet
      _ -> error "Unknown NetworkId"
  poke ptr Mainnet = poke (castPtr ptr) (0 :: Word8)
  poke ptr Testnet = poke (castPtr ptr) (1 :: Word8)

instance MemPack NetworkId where
  packedByteCount _ = 1

  packM Mainnet = packTagM 0
  packM Testnet = packTagM 1

  unpackM = do
    version <- unpackTagM
    case version of
      0 -> pure Mainnet
      1 -> pure Testnet
      _ -> fail "Unknown NetworkId"
