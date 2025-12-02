{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.SCLS.Namespace.Snapshots where

import Cardano.SCLS.Common
import Codec.CBOR.Cuddle.Huddle
import Text.Heredoc (str)
import Prelude (($))

record_entry :: Rule
record_entry =
  comment
    [str| The key for the entry is one of the following:
                |    ((credential / keyhash32) , (0 / 1 / 2))
                |  where additional value stands for the value type:
                |    - 0 : Coin value
                |    - 1 : Keyhash of an delegation address
                |    - 2 : Pool parameters
                |
                |  the size of the key is 30 bytes, (1 + 28 for tag and 1 for value type)
                |]
    $ "record_entry" =:= snapshot_out

record_key :: Rule
record_key =
  "record_key"
    =:= credential
    / keyhash32

snapshot_out :: Rule
snapshot_out =
  comment
    [str| Value maybe be one of the following:
         |  - Coin value
         |  - Keyhash of an delegation address
         |  - Pool parameters
         |]
    $ "snapshot_out"
      =:= arr [0, a coin]
      / arr [1, a keyhash28]
      / arr [2, a pool_params]

pool_params :: Rule
pool_params =
  "pool_params"
    =:= mp
      [ "cost" ==> coin
      , "pledge" ==> coin
      , "margin" ==> unit_interval
      , "relays" ==> arr [0 <+ a relay]
      , "operator" ==> pool_keyhash
      , "pool_owners" ==> set addr_keyhash
      , "vrf_keyhash" ==> vrf_keyhash
      , "pool_metadata" ==> (pool_metadata / VNil)
      , "reward_account" ==> reward_account
      ]

network_id :: Rule
network_id = "network_id" =:= int 0 / int 1

relay :: Rule
relay =
  "relay"
    =:= sarr [0, a single_host_addr]
    / sarr [1, a single_host_name]
    / sarr [2, a multi_host_name]

single_host_addr :: Named Group
single_host_addr =
  comment [str| A single host address relay |] $
    "single_host_addr"
      =:~ grp
        [ a (port / VNil)
        , a (ipv4 / VNil)
        , a (ipv6 / VNil)
        ]

single_host_name :: Named Group
single_host_name =
  "single_host_name"
    =:~ grp [a (port / VNil), a dns_name]

multi_host_name :: Named Group
multi_host_name =
  "multi_host_name" =:~ grp [a dns_name]

pool_metadata :: Rule
pool_metadata = "pool_metadata" =:= arr [a url, a pool_metadata_hash]

pool_metadata_hash :: Rule
pool_metadata_hash = "pool_metadata_hash" =:= VBytes
