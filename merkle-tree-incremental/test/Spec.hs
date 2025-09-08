import Crypto.Hash.Algorithms (SHA3_256)
import Crypto.Hash.MerkleTree (mkMerkleTree, mtHash)
import Crypto.Hash.MerkleTreeIncremental (getMerkleRootHex)
import Crypto.Hash.MerkleTreeIncremental.MerkleTreeState
import Data.ByteString.Char8 qualified as C

import Test.QuickCheck

prop_merkle_root_equal :: [String] -> Bool
prop_merkle_root_equal entries =
    (getMerkleRootHex merkleTree1) == (mtHash merkleTree2)
  where
    entries_bytes = map C.pack entries
    state = foldl add (empty :: MerkleTreeState SHA3_256) entries_bytes
    merkleTree1 = finalize state
    merkleTree2 = mkMerkleTree entries_bytes

main :: IO ()
main = do
    quickCheck prop_merkle_root_equal
