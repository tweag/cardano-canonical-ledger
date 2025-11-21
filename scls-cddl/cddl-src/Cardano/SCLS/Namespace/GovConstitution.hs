{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.SCLS.Namespace.GovConstitution where

import Cardano.SCLS.Common
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Text.Heredoc (str)

record_entry :: Rule
record_entry =
  comment
    [str| Constinution record entry
        | Key is the epoch number (8 bytes)
        |]
    $ "record_entry" =:= constitution

anchor :: Rule
anchor =
  "anchor"
    =:= arr
      [ "anchor_url" ==> url
      , "anchor_data_hash" ==> hash32
      ]

constitution :: Rule
constitution =
  "constitution"
    =:= arr
      [ a anchor
      , a (script_hash / VNil)
      ]
