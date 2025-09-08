{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.SCLS.MerkleTree.Constructor
  ( Constructor,
    empty,
    add,
    finalize,
  )
where

import Cardano.SCLS.MerkleTree (MerkleHash, MerkleTree (MerkleTreeEmpty, MerkleTreeRoot), leafHash, nodeHash)
import Crypto.Hash (HashAlgorithm)
import qualified Data.ByteString as B

newtype Constructor a = Constructor [ConstructorNode a]

data ConstructorNode a = ConstructorNode
  { cLevel :: Int,
    cHash :: MerkleHash a
  }
  deriving (Show)

empty :: Constructor a
empty = Constructor []

add :: (HashAlgorithm a) => Constructor a -> B.ByteString -> Constructor a
add (Constructor constructor) bytes =
  join (Constructor (((ConstructorNode {cLevel = 0, cHash = (leafHash bytes)})) : constructor))

join :: (HashAlgorithm a) => Constructor a -> Constructor a
join (Constructor ((ConstructorNode level1 hash1) : (ConstructorNode level2 hash2) : xs))
  | level1 == level2 = join (Constructor (ConstructorNode {cLevel = (level1 + 1), cHash = (nodeHash hash2 hash1)} : xs))
join c = c

finalize :: (HashAlgorithm a) => Constructor a -> MerkleTree a
finalize (Constructor []) = MerkleTreeEmpty
finalize (Constructor [ConstructorNode _ hash]) =
  MerkleTreeRoot hash
finalize (Constructor (ConstructorNode level1 hash1 : node2@(ConstructorNode level2 hash2) : xs))
  | level1 == level2 = finalize (Constructor (ConstructorNode (level1 + 1) (nodeHash hash2 hash1) : xs))
  | otherwise = finalize (Constructor (ConstructorNode (level1 + 1) hash1 : node2 : xs))
