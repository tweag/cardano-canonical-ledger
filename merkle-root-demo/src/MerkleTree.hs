module MerkleTree
  ( MerkleTree (..),
    MerkleHash,
    merkleHash,
    merkleTreeHash,
  )
where

import Crypto.Hash (Digest, SHA3_256, hash)
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B

type MerkleHash = B.ByteString

data MerkleTree
  = MerkleTreeEmpty
  | MerkleTreeRoot MerkleHash
  deriving (Show, Eq)

merkleTreeHash :: MerkleTree -> MerkleHash
merkleTreeHash MerkleTreeEmpty = merkleHash mempty
merkleTreeHash (MerkleTreeRoot h) = h

sha256 :: B.ByteString -> B.ByteString
sha256 x = BA.convertToBase BA.Base16 (hash x :: Digest SHA3_256)

-- | Hash function to use for merkle tree
merkleHash :: B.ByteString -> B.ByteString
merkleHash = sha256
