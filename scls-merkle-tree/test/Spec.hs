import Cardano.SCLS.MerkleTree (getMerkleHash, merkleTreeHash)
import Cardano.SCLS.MerkleTree.Constructor
import Crypto.Hash.Algorithms (SHA3_256)
import Crypto.Hash.MerkleTree (mkMerkleTree, mtHash)
import Data.ByteString.Char8 qualified as C
import Test.QuickCheck

prop_merkle_root_equal :: [String] -> Bool
prop_merkle_root_equal entries =
    (getMerkleHash $ merkleTreeHash merkleTree1) == mtHash merkleTree2
  where
    entries_bytes = map C.pack entries
    constructor = foldl add (empty :: Constructor SHA3_256) entries_bytes
    merkleTree1 = finalize constructor
    merkleTree2 = mkMerkleTree entries_bytes

main :: IO ()
main = do
    quickCheck prop_merkle_root_equal
