{-# LANGUAGE OverloadedStrings #-}

module Cardano.SCLS.CDDL (
  NamespaceInfo (..),
  namespaces,
) where

import Cardano.SCLS.Namespace.Blocks qualified as Blocks
import Cardano.SCLS.Namespace.GovConstitution qualified as GovConstitution
import Cardano.SCLS.Namespace.GovPParams qualified as GovPParams
import Cardano.SCLS.Namespace.PoolStake qualified as PoolStake
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
    , ("pool_stake/v0", NamespaceInfo (collectFromInit [HIRule PoolStake.record_entry]) 28) -- 28 bytes for key
    , ("snapshots/v0", NamespaceInfo (collectFromInit [HIRule Snapshots.record_entry]) 30)
    , ("gov/constitution/v0", NamespaceInfo (collectFromInit [HIRule GovConstitution.record_entry]) 1)
    , ("gov/pparams/v0", NamespaceInfo (collectFromInit [HIRule GovPParams.record_entry]) 4)
    ]
