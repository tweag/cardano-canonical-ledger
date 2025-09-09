module Main (main) where

import Cardano.SCLS.CDDL (namespaces)
import Cardano.SCLS.Internal.Reader (withNamespacedData)
import Cardano.SCLS.Internal.Serializer.MemPack
import Cardano.SCLS.Internal.Serializer.Reference.Impl (serialize)
import Cardano.Types.Network (NetworkId (..))
import Cardano.Types.SlotNo (SlotNo (..))
import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm')
import Codec.CBOR.Cuddle.CDDL (CDDL, Name (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (
  asMap,
  buildMonoCTree,
  buildRefCTree,
  buildResolvedCTree,
 )
import Codec.CBOR.Cuddle.Huddle
import Codec.CBOR.Term (encodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import Control.Monad (replicateM)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Streaming.Prelude qualified as S
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Random.Stateful (applyAtomicGen, globalStdGen)
import Test.HUnit

main :: IO ()
main = runTestTTAndExit tests
 where
  tests =
    TestList
      [ roundTriptests
      -- basic tests: network encoding, slot encoding
      -- test hash of entire content
      -- test hash of each namespace
      ]
  roundTriptests =
    TestLabel "Roundtrip tests" $
      TestList
        [ TestLabel n $ TestCase $ roundtrip (T.pack n) (toCDDL huddle)
        | (n, huddle) <- Map.toList namespaces
        ]
  roundtrip :: Text -> CDDL -> Assertion
  roundtrip namespace cddl = do
    case buildMonoCTree =<< buildResolvedCTree (buildRefCTree $ asMap cddl) of
      Left err -> assertFailure $ "Failed to build CTree: " ++ show err
      Right mt -> withSystemTempDirectory "scls-format-test-XXXXXX" $ \fn -> do
        data_ <-
          replicateM 1024 $
            applyAtomicGen (generateCBORTerm' mt (Name (T.pack "record_entry") mempty)) globalStdGen
        let encoded_data = [toStrictByteString (encodeTerm term) | term <- data_]
        let fileName = (fn </> "data.scls")
        serialize
          fileName
          Mainnet
          (SlotNo 1)
          namespace
          (fmap (RawBytes) encoded_data)
        withNamespacedData
          fileName
          namespace
          ( \stream -> do
              decoded_data <- S.toList_ stream
              assertEqual
                "Stream roundtrip successful"
                [b | RawBytes b <- decoded_data]
                (sort encoded_data)
          )
