import Constructor
import Crypto.Hash.MerkleTree (mkMerkleTree, mtHash)
import Entry (Entry, serialize)
import MerkleTree (merkleTreeHash)
import Test.QuickCheck

prop_merkle_root_equal :: [Entry] -> Bool
prop_merkle_root_equal entries =
  let constructor = foldl add empty entries
      merkleTree1 = finalize constructor
      merkleTree2 = mkMerkleTree (map serialize entries)
   in merkleTreeHash merkleTree1 == mtHash merkleTree2

main :: IO ()
main = do
  quickCheck prop_merkle_root_equal
