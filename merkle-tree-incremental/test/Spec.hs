import Crypto.Hash.Algorithms
import Crypto.Hash.MerkleTree
import Crypto.Hash.MerkleTree.Incremental
import Data.ByteString.Char8 qualified as C

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

main :: IO ()
main = do
  hspec $
    describe "Incremental Merkle Tree" $ do
      prop "computed Merkle root hash should equal reference implementation one" $ do
        \entries -> do
          let state = foldl add (empty SHA3_256) entries
              merkleTree1 = finalize state
              merkleTree2 = mkMerkleTree entries
          (C.pack $ show $ merkleRootHash merkleTree1) `shouldBe` (mtHash merkleTree2)
