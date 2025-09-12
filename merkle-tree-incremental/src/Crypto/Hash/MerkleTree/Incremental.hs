module Crypto.Hash.MerkleTree.Incremental (
  MerkleTree (..),
  MerkleHash,
  merkleRootHash,
  encodeMerkleHashHex,
  empty,
  add,
  finalize,
)
where

import Crypto.Hash (Digest, HashAlgorithm, hash, hashFinalize, hashInit, hashUpdate, hashUpdates)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray.Encoding qualified as BA
import Data.ByteString qualified as B

-- ----------------------------------------------------------------
-- Merkle Tree and Hashes

type MerkleHash a = Digest a

encodeMerkleHashHex :: (HashAlgorithm a) => MerkleHash a -> B.ByteString
encodeMerkleHashHex = BA.convertToBase BA.Base16

data MerkleTree a
  = MerkleTreeEmpty
  | MerkleTreeRoot (MerkleHash a)
  deriving (Show, Eq)

merkleRootHash :: forall a. (HashAlgorithm a) => MerkleTree a -> MerkleHash a
merkleRootHash MerkleTreeEmpty = hash B.empty
merkleRootHash (MerkleTreeRoot h) = h

-- | Hash function to use for merkle tree
leafHash :: forall a b. (HashAlgorithm a, ByteArrayAccess b) => b -> MerkleHash a
leafHash b =
  hashFinalize $ flip hashUpdate b $ hashUpdate hashInit $ B.singleton 0

nodeHash :: forall a. (HashAlgorithm a) => MerkleHash a -> MerkleHash a -> MerkleHash a
nodeHash h1 h2 =
  hashFinalize $ flip hashUpdates [encodeMerkleHashHex h1, encodeMerkleHashHex h2] $ hashUpdate hashInit $ B.singleton 1

-- ----------------------------------------------------------------
-- Incremental Merkle Tree construction

newtype MerkleTreeState a = MerkleTreeState [MerkleTreeStateNode a]

data MerkleTreeStateNode a = MerkleTreeStateNode
  { cLevel :: Int
  , cHash :: MerkleHash a
  }
  deriving (Show)

empty :: (HashAlgorithm a) => a -> MerkleTreeState a
empty _ = MerkleTreeState []

add :: (HashAlgorithm a, ByteArrayAccess b) => MerkleTreeState a -> b -> MerkleTreeState a
add (MerkleTreeState state) bytes =
  join (MerkleTreeState (MerkleTreeStateNode{cLevel = 0, cHash = leafHash bytes} : state))

join :: (HashAlgorithm a) => MerkleTreeState a -> MerkleTreeState a
join (MerkleTreeState ((MerkleTreeStateNode level1 hash1) : (MerkleTreeStateNode level2 hash2) : xs))
  | level1 == level2 = join (MerkleTreeState (MerkleTreeStateNode{cLevel = level1 + 1, cHash = nodeHash hash2 hash1} : xs))
join state = state

finalize :: (HashAlgorithm a) => MerkleTreeState a -> MerkleTree a
finalize (MerkleTreeState []) = MerkleTreeEmpty
finalize (MerkleTreeState [MerkleTreeStateNode _ merkleHash]) =
  MerkleTreeRoot merkleHash
finalize (MerkleTreeState (MerkleTreeStateNode level1 hash1 : node2@(MerkleTreeStateNode level2 hash2) : xs))
  | level1 == level2 = finalize (MerkleTreeState (MerkleTreeStateNode (level1 + 1) (nodeHash hash2 hash1) : xs))
  | otherwise = finalize (MerkleTreeState (MerkleTreeStateNode (level1 + 1) hash1 : node2 : xs))
