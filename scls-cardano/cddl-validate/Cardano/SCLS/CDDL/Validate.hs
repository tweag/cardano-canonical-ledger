{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

-- | Various helper functions for CBOR validation against supported CDDL specifications.
module Cardano.SCLS.CDDL.Validate (
  validateTermAgainst,
  validateBytesAgainst,
  invalidSpecs,
) where

import Cardano.SCLS.CDDL
import Codec.CBOR.Cuddle.CBOR.Validator (CBORTermResult (..), validateTerm)
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
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term (Term, decodeTerm)
import Control.Monad.Trans.Reader (runReader)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Functor ((<&>))
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

-- | Validate a parsed CBOR term against a rule in the namespace.
validateTermAgainst :: Term -> Text -> Text -> Maybe CBORTermResult
validateTermAgainst term namespace name =
  let cddlName = Name name
   in Map.lookup namespace validSpecs >>= \cddl@(CTreeRoot cddlTree) ->
        Map.lookup cddlName cddlTree <&> \rule ->
          runReader (validateTerm term (runIdentity rule)) cddl

-- | Validate raw bytes against a rule in the namespace.
validateBytesAgainst :: ByteString -> Text -> Text -> Maybe CBORTermResult
validateBytesAgainst bytes namespace name =
  case CBOR.deserialiseFromBytes decodeTerm (BSL.fromStrict bytes) of
    Left _ -> Nothing
    Right (_, term) -> validateTermAgainst term namespace name
