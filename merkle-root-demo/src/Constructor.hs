module Constructor
  ( Constructor,
    empty,
    add,
    finalize,
  )
where

import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Entry (Entry, serialize)
import MerkleTree (MerkleHash, MerkleTree (MerkleTreeEmpty, MerkleTreeRoot), merkleHash)

type Constructor = [ConstructorNode]

data ConstructorNode = ConstructorNode
  { cLevel :: Int,
    cHash :: MerkleHash
  }
  deriving (Show)

empty :: Constructor
empty = []

leafHash :: Entry -> B.ByteString
leafHash entry =
  merkleHash (B.singleton 0 <> BA.convert (serialize entry))

hashPair :: B.ByteString -> B.ByteString -> B.ByteString
hashPair h1 h2 = merkleHash $ mconcat $ [B.singleton 1, BA.convert h1, BA.convert h2]

add :: Constructor -> Entry -> Constructor
add constructor entry =
  join (((ConstructorNode {cLevel = 0, cHash = (leafHash entry)})) : constructor)

join :: Constructor -> Constructor
join ((ConstructorNode level1 hash1) : (ConstructorNode level2 hash2) : xs)
  | level1 == level2 = join (ConstructorNode {cLevel = (level1 + 1), cHash = (hashPair hash2 hash1)} : xs)
join c = c

finalize :: Constructor -> MerkleTree
finalize [] = MerkleTreeEmpty
finalize [ConstructorNode _ hash] =
  MerkleTreeRoot hash
finalize (ConstructorNode level1 hash1 : node2@(ConstructorNode level2 hash2) : xs)
  | level1 == level2 = finalize (ConstructorNode (level1 + 1) (hashPair hash2 hash1) : xs)
  | otherwise = finalize (ConstructorNode (level1 + 1) hash1 : node2 : xs)
