{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Util.Debug where

import Cardano.SCLS.CDDL

-- import Cardano.SCLS.CDDL (NamespaceInfo (..), namespaces)
import Cardano.SCLS.Internal.Serializer.Dump.Plan (addChunks', defaultSerializationPlan')
import Cardano.SCLS.Internal.Serializer.External.Impl qualified as External (serialize)
import Cardano.SCLS.Internal.Serializer.MemPack
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Cardano.Types.Network (NetworkId (..))
import Cardano.Types.SlotNo (SlotNo (..))
import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm')
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot')
import Codec.CBOR.Cuddle.CDDL.Resolve (
  MonoRef,
  asMap,
  buildMonoCTree,
  buildRefCTree,
  buildResolvedCTree,
 )
import Codec.CBOR.Cuddle.Huddle
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Streaming.Prelude qualified as S
import System.Random.Stateful (applyAtomicGen, globalStdGen, uniformByteStringM)

import Cardano.SCLS.Internal.Entry.CBOREntry (GenericCBOREntry (GenericCBOREntry), SomeCBOREntry (SomeCBOREntry))
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (ChunkEntry))
import Cardano.SCLS.Util.Result
import GHC.TypeNats

-- | Generate a scls file with random data for debugging purposes.
generateDebugFile :: FilePath -> [(Namespace, Maybe Int)] -> IO Result
generateDebugFile outputFile namespaceEntries = do
  _ <-
    External.serialize
      outputFile
      Mainnet
      (SlotNo 1)
      ( defaultSerializationPlan'
          & addChunks' do
            S.each
              ( namespaceEntries <&> \(namespace, mCount) ->
                  case Map.lookup (Namespace.asText namespace) namespaces of
                    Nothing -> error $ "Unknown namespace: " ++ Namespace.asString namespace
                    Just NamespaceInfo{..} -> do
                      case buildMonoCTree =<< buildResolvedCTree (buildRefCTree $ asMap $ toCDDL namespaceSpec) of
                        Left err -> error $ "Failed to parse cuddle specification: " ++ show err
                        Right mt ->
                          withSomeSNat namespaceKeySize \(snat :: SNat n) -> do
                            withKnownNat snat do
                              ( namespace
                                  S.:> (generateNamespaceEntries @n (fromMaybe 16 mCount) mt & S.map SomeCBOREntry)
                                )
              )
      )
  pure Ok

generateNamespaceEntries :: forall n. (KnownNat n) => Int -> CTreeRoot' Identity MonoRef -> S.Stream (S.Of (GenericCBOREntry n)) IO ()
generateNamespaceEntries count spec = replicateM_ count do
  let size = fromSNat @n SNat
  keyIn <- liftIO $ uniformByteStringM (fromIntegral size) globalStdGen
  term <- liftIO $ applyAtomicGen (generateCBORTerm' spec (Name (T.pack "record_entry") mempty)) globalStdGen
  S.yield $ GenericCBOREntry $ ChunkEntry (ByteStringSized @n keyIn) (CBORTerm term)
