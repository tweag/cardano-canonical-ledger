module Crypto.Hash.MerkleTreeIncremental.MerkleTreeState (
    MerkleTreeState,
    empty,
    add,
    finalize,
)
where

import Crypto.Hash (HashAlgorithm)
import Crypto.Hash.MerkleTreeIncremental (MerkleHash, MerkleTree (MerkleTreeEmpty, MerkleTreeRoot), leafHash, nodeHash)
import Data.ByteArray (ByteArrayAccess)

newtype MerkleTreeState a = MerkleTreeState [MerkleTreeStateNode a]

data MerkleTreeStateNode a = MerkleTreeStateNode
    { cLevel :: Int
    , cHash :: MerkleHash a
    }
    deriving (Show)

empty :: MerkleTreeState a
empty = MerkleTreeState []

add :: (HashAlgorithm a, ByteArrayAccess b) => MerkleTreeState a -> b -> MerkleTreeState a
add (MerkleTreeState state) bytes =
    join (MerkleTreeState (MerkleTreeStateNode{cLevel = 0, cHash = leafHash bytes} : state))

join :: (HashAlgorithm a) => MerkleTreeState a -> MerkleTreeState a
join (MerkleTreeState ((MerkleTreeStateNode level1 hash1) : (MerkleTreeStateNode level2 hash2) : xs))
    | level1 == level2 = join (MerkleTreeState (MerkleTreeStateNode{cLevel = level1 + 1, cHash = nodeHash hash2 hash1} : xs))
join state = state

finalize :: (HashAlgorithm a) => MerkleTreeState a -> MerkleTree a
finalize (MerkleTreeState []) = MerkleTreeEmpty
finalize (MerkleTreeState [MerkleTreeStateNode _ hash]) =
    MerkleTreeRoot hash
finalize (MerkleTreeState (MerkleTreeStateNode level1 hash1 : node2@(MerkleTreeStateNode level2 hash2) : xs))
    | level1 == level2 = finalize (MerkleTreeState (MerkleTreeStateNode (level1 + 1) (nodeHash hash2 hash1) : xs))
    | otherwise = finalize (MerkleTreeState (MerkleTreeStateNode (level1 + 1) hash1 : node2 : xs))
