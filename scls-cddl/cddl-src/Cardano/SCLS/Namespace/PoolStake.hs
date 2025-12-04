{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.SCLS.Namespace.PoolStake where

import Cardano.SCLS.Common
import Codec.CBOR.Cuddle.Huddle
import Text.Heredoc (str)
import Prelude (($))

record_entry :: Rule
record_entry =
  comment
    [str| The key for the entry is one of the following:
        |
        | ```
        | meta:
        |   endian: be
        |
        | seq:
        |   - id: key
        |     type: pool_stake
        |
        | types:
        |   pool_stake:
        |     seq:
        |       - id: keyhash
        |         doc: stake pool keyhash
        |         size: 28
        | ```
        |
        |]
    $ "record_entry" =:= individual_pool_stake

individual_pool_stake :: Rule
individual_pool_stake =
  comment
    [str|Individual pool stake information
        |
        |Fields:
        |  - vrf: VRF verification key hash for the pool
        |  - total: Total stake delegated to the pool
        |]
    $ "individual_pool_stake"
      =:= mp
        [ "vrf" ==> vrf_keyhash
        , "total" ==> coin
        ]
