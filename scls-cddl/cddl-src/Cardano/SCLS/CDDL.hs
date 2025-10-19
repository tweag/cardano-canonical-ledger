module Cardano.SCLS.CDDL where

import Cardano.SCLS.Namespace.UTxO qualified as UTxO
import Codec.CBOR.Cuddle.Huddle (Huddle, HuddleItem (HIRule), collectFromInit)
import Data.Map.Strict qualified as Map

namespaces :: Map.Map String Huddle
namespaces =
  Map.fromList
    [("utxo/v0", collectFromInit [HIRule UTxO.record_entry])]
