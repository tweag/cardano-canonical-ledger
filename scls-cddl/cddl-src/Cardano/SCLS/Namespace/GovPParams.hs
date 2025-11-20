{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.SCLS.Namespace.GovPParams where

import Cardano.SCLS.Common
import Cardano.SCLS.Common ()
import Codec.CBOR.Cuddle.Huddle
import Data.Word (Word64)
import Text.Heredoc (str)
import Prelude (($))

record_entry :: Rule
record_entry =
  comment
    [str| The key for the entry is one of the following:
                |    prev / curr / fut1 / fut0
                |      fut0 stands for possible future
                |      fut1 stands for definite future
                |
                | fut0 + no pparams is not represented, key should be omitted in that case
                |]
    $ "record_entry" =:= gov_pparams_out

gov_pparams_out :: Rule
gov_pparams_out =
  "gov_pparams_out"
    =:= mp
      [ "max_block_size_words" ==> VUInt
      , "max_header_size_words" ==> VUInt
      , "max_tx_size_words" ==> VUInt
      , "max_proposal_size_words" ==> VUInt
      , "blocks_signing_fraction" ==> unit_interval
      , "slots_per_epoch" ==> VUInt
      , "update_ttl_slots" ==> VUInt
      , "scriptversion" ==> VUInt
      , "update_adapt_threshold" ==> unit_interval
      , "factora" ==> (VUInt `sized` (8 :: Word64))
      , "factorb" ==> (VUInt `sized` (8 :: Word64))
      ]
