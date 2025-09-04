module Cardano.SCLS.CDDL where

import qualified Cardano.SCLS.Namespace.UTxO as UTxO
import qualified Data.Map.Strict as Map
import Codec.CBOR.Cuddle.Huddle (Rule)

namespaces :: Map.Map String Rule
namespaces = Map.fromList
  [ ("utxo", UTxO.record_entry)]
