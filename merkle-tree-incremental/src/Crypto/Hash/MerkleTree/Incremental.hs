module Crypto.Hash.MerkleTree.Incremental (
    MerkleTree (..),
    MerkleHash,
    merkleTreeHash,
    leafHash,
    nodeHash,
    getMerkleRootHex,
)
where

import Crypto.Hash (Digest, HashAlgorithm, hash, hashFinalize, hashInit, hashUpdate, hashUpdates)
import Data.ByteArray as BA
import Data.ByteArray.Encoding qualified as BA
import Data.ByteString qualified as B

type MerkleHash a = Digest a

merkleHashHex :: (HashAlgorithm a) => MerkleHash a -> B.ByteString
merkleHashHex = BA.convertToBase BA.Base16

data MerkleTree a
    = MerkleTreeEmpty
    | MerkleTreeRoot (MerkleHash a)
    deriving (Show, Eq)

getMerkleRootHex :: (HashAlgorithm a) => MerkleTree a -> B.ByteString
getMerkleRootHex = merkleHashHex . merkleTreeHash

merkleTreeHash :: forall a. (HashAlgorithm a) => MerkleTree a -> MerkleHash a
merkleTreeHash MerkleTreeEmpty = hash B.empty
merkleTreeHash (MerkleTreeRoot h) = h

-- | Hash function to use for merkle tree
leafHash :: forall a b. (HashAlgorithm a, ByteArrayAccess b) => b -> MerkleHash a
leafHash b =
    hashFinalize $ flip hashUpdate b $ hashUpdate hashInit $ B.singleton 0

nodeHash :: forall a. (HashAlgorithm a) => MerkleHash a -> MerkleHash a -> MerkleHash a
nodeHash h1 h2 =
    hashFinalize $ flip hashUpdates [merkleHashHex h1, merkleHashHex h2] $ hashUpdate hashInit $ B.singleton 1
