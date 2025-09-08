module Cardano.SCLS.MerkleTree (
    MerkleTree (..),
    MerkleHash,
    merkleTreeHash,
    leafHash,
    nodeHash,
    getMerkleHash,
)
where

import Crypto.Hash (HashAlgorithm, hash)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as BA
import Data.ByteString qualified as B

newtype MerkleHash a = MerkleHash
    { getMerkleHash ::
        B.ByteString
    }
    deriving (Show, Eq)

mkMerkleHash :: B.ByteString -> MerkleHash a
mkMerkleHash h =
    MerkleHash
        { getMerkleHash = h
        }

instance ByteArrayAccess (MerkleHash a) where
    length = B.length . getMerkleHash
    withByteArray (MerkleHash bs) f = BA.withByteArray bs f

data MerkleTree a
    = MerkleTreeEmpty
    | MerkleTreeRoot (MerkleHash a)
    deriving (Show, Eq)

merkleTreeHash :: forall a. (HashAlgorithm a) => MerkleTree a -> MerkleHash a
merkleTreeHash MerkleTreeEmpty = merkleHash mempty
merkleTreeHash (MerkleTreeRoot h) = h

merkleHash :: forall a. (HashAlgorithm a) => B.ByteString -> MerkleHash a
merkleHash bytes = mkMerkleHash $ BA.convertToBase BA.Base16 (hash @_ @a bytes)

-- | Hash function to use for merkle tree
leafHash :: forall a. (HashAlgorithm a) => B.ByteString -> MerkleHash a
leafHash bytes =
    merkleHash @a (B.singleton 0 <> BA.convert bytes)

nodeHash :: forall a. (HashAlgorithm a) => MerkleHash a -> MerkleHash a -> MerkleHash a
nodeHash h1 h2 = merkleHash @a $ mconcat $ [B.singleton 1, BA.convert h1, BA.convert h2]
