{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Cardano.SCLS.Namespace.Pots where

import Cardano.SCLS.Common
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Text.Heredoc (str)

-- | Top-level entry for pots namespace
record_entry :: Rule
record_entry = "record_entry" =:= pots_table

-- | Table mapping pot names to coin values
pots_table :: Rule
pots_table =
  comment
    [str|Pots table containing the various accounting pots in Cardano
        |
        |Fields:
        |  - treasury: coins in the treasury
        |  - reserves: coins in the reserves
        |  - deposit: total deposits held
        |  - fee: accumulated fees
        |  - donation: donations pot
        |]
    $ "pots_table"
      =:= mp
        [ "fee" ==> coin
        , "deposit" ==> coin
        , "donation" ==> coin
        , "reserves" ==> coin
        , "treasury" ==> coin
        ]
