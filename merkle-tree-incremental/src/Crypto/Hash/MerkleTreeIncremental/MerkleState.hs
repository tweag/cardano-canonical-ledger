module Crypto.Hash.MerkleTreeIncremental.MerkleState (
    MerkleState,
    empty,
    add,
    finalize,
)
where

import Crypto.Hash (HashAlgorithm)
import Crypto.Hash.MerkleTreeIncremental (MerkleHash, MerkleTree (MerkleTreeEmpty, MerkleTreeRoot), leafHash, nodeHash)
import Data.ByteArray (ByteArrayAccess)

newtype MerkleState a = MerkleState [MerkleStateNode a]

data MerkleStateNode a = MerkleStateNode
    { cLevel :: Int
    , cHash :: MerkleHash a
    }
    deriving (Show)

empty :: MerkleState a
empty = MerkleState []

add :: (HashAlgorithm a, ByteArrayAccess b) => MerkleState a -> b -> MerkleState a
add (MerkleState state) bytes =
    join (MerkleState (MerkleStateNode{cLevel = 0, cHash = leafHash bytes} : state))

join :: (HashAlgorithm a) => MerkleState a -> MerkleState a
join (MerkleState ((MerkleStateNode level1 hash1) : (MerkleStateNode level2 hash2) : xs))
    | level1 == level2 = join (MerkleState (MerkleStateNode{cLevel = level1 + 1, cHash = nodeHash hash2 hash1} : xs))
join state = state

finalize :: (HashAlgorithm a) => MerkleState a -> MerkleTree a
finalize (MerkleState []) = MerkleTreeEmpty
finalize (MerkleState [MerkleStateNode _ hash]) =
    MerkleTreeRoot hash
finalize (MerkleState (MerkleStateNode level1 hash1 : node2@(MerkleStateNode level2 hash2) : xs))
    | level1 == level2 = finalize (MerkleState (MerkleStateNode (level1 + 1) (nodeHash hash2 hash1) : xs))
    | otherwise = finalize (MerkleState (MerkleStateNode (level1 + 1) hash1 : node2 : xs))
