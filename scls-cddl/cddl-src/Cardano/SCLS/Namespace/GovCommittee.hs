{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.SCLS.Namespace.GovCommittee where

import Cardano.SCLS.Common
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Word (Word64)
import Text.Heredoc (str)

record_entry :: Rule
record_entry =
  comment
    [str| The key for the entry is  epoch number (8 bytes) |]
    $ "record_entry" =:= committee

committee_cold_credential :: Rule
committee_cold_credential = "committee_cold_credential" =:= credential

committee :: Rule
committee =
  "committee"
    =:= arr
      [ a (mp [0 <+ asKey committee_cold_credential ==> epoch_no])
      , a unit_interval
      ]

epoch_no :: Rule
epoch_no = "epoch_no" =:= VUInt `sized` (8 :: Word64)
