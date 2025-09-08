{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Cardano.SCLS.Namespace.UTxO where

import Codec.CBOR.Cuddle.Huddle
import Codec.CBOR.Cuddle.Comments ((//-))
import Cardano.SCLS.Common
import Text.Heredoc (str)
import Data.Function (($))
import Data.Word (Word)


record_entry :: Rule
record_entry = "record_entry" =:= generic_record tx_in tx_out

tx_in :: Rule
tx_in = "tx_in" =:= arr [
    a hash32,
    a (VUInt `sized` (2 :: Word))
  ]

tx_out :: Rule
tx_out = "tx_out" =:= arr [0, a shelley_tx_out] / arr [1, a babbage_tx_out]

shelley_tx_out :: Rule
shelley_tx_out = "shelley_tx_out" =:= 
    arr [a address, "amount" ==> value, opt ("datum_hash" ==> hash32)]


babbage_tx_out :: Rule
babbage_tx_out =
  comment
    [str|NEW starting with babbage
        |  datum_option
        |  script_ref
        |]
    $ "babbage_tx_out"
      =:= mp
        [ idx 0 ==> address
        , idx 1 ==> value
        , opt $ idx 2 ==> datum_option
        , opt $ idx 3 ==> ("script_ref" =:= tag 24 (VBytes `cbor` script))
        ]

plutus_data :: Rule
plutus_data =
  "plutus_data"
    =:= constr plutus_data
    / smp [0 <+ asKey plutus_data ==> plutus_data]
    / sarr [0 <+ a plutus_data]
    / big_int
    / bounded_bytes

data' :: Rule
data' = "data" =:= tag 24 (VBytes `cbor` plutus_data)        

datum_option :: Rule
datum_option = "datum_option" =:= arr [0, a hash32] / arr [1, a data']

constr :: IsType0 x => x -> GRuleCall
constr = binding $ \x ->
  comment
    [str|NEW
        |  #6.102([uint, [* a]]): For tag range 6.1280 .. 6.1400 inclusive
        |]
    $ "constr"
      =:= tag 121 (arr [0 <+ a x])
      / tag 122 (arr [0 <+ a x])
      / tag 123 (arr [0 <+ a x])
      / tag 124 (arr [0 <+ a x])
      / tag 125 (arr [0 <+ a x])
      / tag 126 (arr [0 <+ a x])
      / tag 127 (arr [0 <+ a x])
      / tag 102 (arr [a VUInt, a $ arr [0 <+ a x]])

script :: Rule
script =
  "script"
    =:= arr [0, a native_script]
    / (arr [1, a VBytes] //- "Plutus V1")
    / (arr [2, a VBytes] //- "Plutus V2")
    / (arr [3, a VBytes] //- "Plutus V3")
    
native_script :: Rule
native_script =
  "native_script"
    =:= arr [a script_pubkey]
    / arr [a script_all]
    / arr [a script_any]
    / arr [a script_n_of_k]
    / arr [a invalid_before]
    -- Timelock validity intervals are half-open intervals [a, b).
    -- This field specifies the left (included) endpoint a.
    / arr [a invalid_hereafter]

script_pubkey :: Named Group
script_pubkey = "script_pubkey" =:~ grp [0, a hash28]

script_all :: Named Group
script_all = "script_all" =:~ grp [1, a (arr [0 <+ a native_script])]

script_any :: Named Group
script_any = "script_any" =:~ grp [2, a (arr [0 <+ a native_script])]

script_n_of_k :: Named Group
script_n_of_k =
  "script_n_of_k"
    =:~ grp [3, "n" ==> int64, a (arr [0 <+ a native_script])]

invalid_before :: Named Group
invalid_before = "invalid_before" =:~ grp [4, a slot_no]

invalid_hereafter :: Named Group
invalid_hereafter = "invalid_hereafter" =:~ grp [5, a slot_no]
    
