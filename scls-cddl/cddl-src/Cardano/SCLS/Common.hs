{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Common CDDL Definitions
module Cardano.SCLS.Common where

import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Int (Int64)
import Data.Word
import GHC.Integer (Integer)
import GHC.Real (Integral (toInteger))

-- after drop of the GHC-9.10 we can switch to Data.Bounded
import Text.Heredoc
import Prelude (Bounded (..))

--------------------------------------------------------------------------------
-- Coins and Assets
--------------------------------------------------------------------------------

coin :: Rule
coin = "coin" =:= VUInt

positive_coin :: Rule
positive_coin =
  "positive_coin"
    =:= (1 :: Integer)
    ... toInteger (maxBound @Word64)

policy_id :: Rule
policy_id = "policy_id" =:= hash28

asset_name :: Rule
asset_name = "asset_name" =:= VBytes `sized` (0 :: Word64, 32 :: Word64)

multiasset :: (IsType0 a) => a -> GRuleCall
multiasset = binding $ \x ->
  "multiasset"
    =:= mp [0 <+ asKey policy_id ==> mp [1 <+ asKey asset_name ==> x]]

value :: Rule
value = "value" =:= coin / sarr [a coin, a (multiasset positive_coin)]

--------------------------------------------------------------------------------
-- Slots and Blocks
--------------------------------------------------------------------------------

slot_no :: Rule
slot_no = "slot_no" =:= VUInt `sized` (8 :: Word)

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

address :: Rule
address = "address" =:= VBytes

--------------------------------------------------------------------------------
-- Crypto
--------------------------------------------------------------------------------

hash28 :: Rule
hash28 = "hash28" =:= VBytes `sized` (28 :: Word64)

hash32 :: Rule
hash32 = "hash32" =:= VBytes `sized` (32 :: Word64)

vkey :: Rule
vkey = "vkey" =:= VBytes `sized` (32 :: Word64)

vrf_vkey :: Rule
vrf_vkey = "vrf_vkey" =:= VBytes `sized` (32 :: Word64)

vrf_cert :: Rule
vrf_cert = "vrf_cert" =:= arr [a VBytes, a (VBytes `sized` (80 :: Word64))]

kes_vkey :: Rule
kes_vkey = "kes_vkey" =:= VBytes `sized` (32 :: Word64)

kes_signature :: Rule
kes_signature = "kes_signature" =:= VBytes `sized` (448 :: Word64)

signkeyKES :: Rule
signkeyKES = "signkeyKES" =:= VBytes `sized` (64 :: Word64)

signature :: Rule
signature = "signature" =:= VBytes `sized` (64 :: Word64)

-------------------------------------------------------------------------------
-- Numbers
--------------------------------------------------------------------------------

big_int :: Rule
big_int = "big_int" =:= VInt / big_uint / big_nint

big_uint :: Rule
big_uint = "big_uint" =:= tag 2 bounded_bytes

big_nint :: Rule
big_nint = "big_nint" =:= tag 3 bounded_bytes

int64 :: Rule
int64 = "int64" =:= toInteger (minBound @Int64) ... toInteger (maxBound @Int64)

-------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

bounded_bytes :: Rule
bounded_bytes =
  comment
    [str|The real bounded_bytes does not have this limit. it instead has
        |a different limit which cannot be expressed in CDDL.
        |
        |The limit is as follows:
        | - bytes with a definite-length encoding are limited to size 0..64
        | - for bytes with an indefinite-length CBOR encoding, each chunk is
        |   limited to size 0..64
        | ( reminder: in CBOR, the indefinite-length encoding of
        | bytestrings consists of a token #2.31 followed by a sequence
        | of definite-length encoded bytestrings and a stop code )
        |]
    $ "bounded_bytes"
      =:= VBytes
      `sized` (0 :: Word64, 64 :: Word64)
