{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.SCLS.Namespace.GovProposals where

import Cardano.SCLS.Common
import Cardano.SCLS.Namespace.GovConstitution (constitution)
import Cardano.SCLS.Namespace.GovPParams
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Word (Word64)
import Text.Heredoc (str)

record_entry :: Rule
record_entry =
  comment
    [str| The key for the entry is a gov_action_id (34 bytes: 32 bytes for transaction_id + 2 bytes for gov_action_index) |]
    $ "record_entry" =:= proposal

committee_cold_credential :: Rule
committee_cold_credential = "committee_cold_credential" =:= credential

proposal :: Rule
proposal =
  "proposal"
    =:= mp
      [ "proposed_in" ==> epoch_no
      , "expires_after" ==> epoch_no
      , "drep_votes" ==> mp [0 <+ asKey credential ==> coin]
      , "stake_pool_votes" ==> mp [0 <+ asKey pool_keyhash ==> coin]
      , "committee_votes" ==> mp [0 <+ asKey committee_cold_credential ==> coin]
      , "proposal_procedure" ==> proposal_procedure
      ]

proposal_procedure :: Rule
proposal_procedure =
  "proposal_procedure"
    =:= mp
      [ "anchor" ==> anchor
      , "deposit" ==> coin
      , "return_address" ==> reward_account
      , "gov_action" ==> gov_action
      ]

gov_action :: Rule
gov_action =
  "gov_action"
    =:= ( arr
            [ 0
            , "purpose" ==> (gov_action_id / VNil)
            , "update" ==> gov_pparams_out
            , "hash" ==> (script_hash / VNil)
            ]
            //- "Params update"
        )
    / (arr [1, a (gov_action_id / VNil), a protocol_version] //- "Hard fork")
    / ( arr
          [ 2
          , "withdrawls" ==> mp [0 <+ asKey reward_account ==> coin]
          , a (script_hash / VNil)
          ]
          //- "Treasury withdraw"
      )
    / (arr [3, "purpose" ==> (gov_action_id / VNil)] //- "No confidence")
    / ( arr
          [ 4
          , "purpose" ==> (gov_action_id / VNil)
          , "removed" ==> arr [0 <+ a credential]
          , "added" ==> mp [0 <+ asKey credential ==> epoch_no]
          , "threshold" ==> unit_interval
          ]
          //- "Committee membership update"
      )
    / (arr [5, "purpose" ==> (gov_action_id / VNil), "constitution" ==> constitution] //- "New constitution")
    / (arr [6, a VNil] //- "Info action")

gov_action_id :: Rule
gov_action_id =
  "gov_action_id"
    =:= arr ["transaction_id" ==> hash32, "gov_action_index" ==> VUInt `sized` (2 :: Word64)]
