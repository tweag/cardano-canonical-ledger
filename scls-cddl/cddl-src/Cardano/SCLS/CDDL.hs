{-# LANGUAGE OverloadedStrings #-}

module Cardano.SCLS.CDDL (
  NamespaceInfo (..),
  namespaces,
) where

import Cardano.SCLS.Namespace.Blocks qualified as Blocks
import Cardano.SCLS.Namespace.Pots qualified as Pots
import Cardano.SCLS.Namespace.Snapshots qualified as Snapshots
import Cardano.SCLS.Namespace.UTxO qualified as UTxO
import Codec.CBOR.Cuddle.Huddle (Huddle, HuddleItem (HIRule), collectFromInit)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Numeric.Natural (Natural)

-- | Various information about supported namespaces.
data NamespaceInfo = NamespaceInfo
  { namespaceSpec :: !Huddle
  -- ^ Specification for the namespace entries.
  , namespaceKeySize :: !Natural
  -- ^ Size of the keys in the namespace.
  }

-- | List of the namespaces known to the SCLS utilities.
namespaces :: Map.Map Text NamespaceInfo
namespaces =
  Map.fromList
    [ ("utxo/v0", NamespaceInfo (collectFromInit [HIRule UTxO.record_entry]) 34)
    , ("blocks/v0", NamespaceInfo (collectFromInit [HIRule Blocks.record_entry]) 36) -- 28 bytes for key, and 8 for epoch in BE
    , ("pots/v0", NamespaceInfo (collectFromInit [HIRule Pots.record_entry]) 8) -- Key is epoch number
    , ("snapshots/v0", NamespaceInfo (collectFromInit [HIRule Snapshots.record_entry]) 30)
    ]
