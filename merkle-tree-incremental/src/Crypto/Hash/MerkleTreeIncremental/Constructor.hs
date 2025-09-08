module Crypto.Hash.MerkleTreeIncremental.Constructor (
    Constructor,
    empty,
    add,
    finalize,
)
where

import Crypto.Hash (HashAlgorithm)
import Crypto.Hash.MerkleTreeIncremental (MerkleHash, MerkleTree (MerkleTreeEmpty, MerkleTreeRoot), leafHash, nodeHash)
import Data.ByteArray (ByteArrayAccess)

newtype Constructor a = Constructor [ConstructorNode a]

data ConstructorNode a = ConstructorNode
    { cLevel :: Int
    , cHash :: MerkleHash a
    }
    deriving (Show)

empty :: Constructor a
empty = Constructor []

add :: (HashAlgorithm a, ByteArrayAccess b) => Constructor a -> b -> Constructor a
add (Constructor constructor) bytes =
    join (Constructor (ConstructorNode{cLevel = 0, cHash = leafHash bytes} : constructor))

join :: (HashAlgorithm a) => Constructor a -> Constructor a
join (Constructor ((ConstructorNode level1 hash1) : (ConstructorNode level2 hash2) : xs))
    | level1 == level2 = join (Constructor (ConstructorNode{cLevel = level1 + 1, cHash = nodeHash hash2 hash1} : xs))
join c = c

finalize :: (HashAlgorithm a) => Constructor a -> MerkleTree a
finalize (Constructor []) = MerkleTreeEmpty
finalize (Constructor [ConstructorNode _ hash]) =
    MerkleTreeRoot hash
finalize (Constructor (ConstructorNode level1 hash1 : node2@(ConstructorNode level2 hash2) : xs))
    | level1 == level2 = finalize (Constructor (ConstructorNode (level1 + 1) (nodeHash hash2 hash1) : xs))
    | otherwise = finalize (Constructor (ConstructorNode (level1 + 1) hash1 : node2 : xs))
