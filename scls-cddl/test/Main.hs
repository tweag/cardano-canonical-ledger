import Cardano.SCLS.CDDL.Validate
import Data.Map.Strict qualified as Map
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Basic checks" $ do
    it "No invalid namespaces" $
      invalidSpecs `shouldSatisfy` Map.null
