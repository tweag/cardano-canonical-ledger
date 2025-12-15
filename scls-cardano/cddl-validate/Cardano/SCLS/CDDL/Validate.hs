{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

-- | Various helper functions for CBOR validation against supported CDDL specifications.
module Cardano.SCLS.CDDL.Validate (
  validateBytesAgainst,
  invalidSpecs,
) where

import Cardano.SCLS.CDDL
import Codec.CBOR.Cuddle.CBOR.Validator (CBORTermResult (..), validateCBOR)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (
  MonoReferenced,
  NameResolutionFailure,
  asMap,
  buildMonoCTree,
  buildRefCTree,
  buildResolvedCTree,
 )
import Codec.CBOR.Cuddle.Huddle (toCDDL)
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (mapIndex), mapCDDLDropExt)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | Pre-compiled CDDL specifications for all supported namespaces.
invalidSpecs :: Map.Map Text NameResolutionFailure
validSpecs :: Map.Map Text (CTreeRoot Codec.CBOR.Cuddle.CDDL.Resolve.MonoReferenced)
(invalidSpecs, validSpecs) = Map.mapEither
  do
    \NamespaceInfo{..} -> do
      case buildMonoCTree =<< buildResolvedCTree (buildRefCTree $ asMap $ mapCDDLDropExt $ toCDDL namespaceSpec) of
        Left e -> Left e
        Right tree -> Right tree
  do namespaces

-- | Validate raw bytes against a rule in the namespace.
validateBytesAgainst :: ByteString -> Text -> Text -> Maybe CBORTermResult
validateBytesAgainst bytes namespace name = do
  cddl <- Map.lookup namespace validSpecs
  pure $ validateCBOR bytes (Name name) (mapIndex cddl)
