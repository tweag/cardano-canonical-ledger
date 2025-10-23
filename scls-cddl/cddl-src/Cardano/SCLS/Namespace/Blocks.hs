{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Cardano.SCLS.Namespace.Blocks where

import Cardano.SCLS.Common
import Codec.CBOR.Cuddle.Huddle

record_entry :: Rule
record_entry = "record_entry" =:= generic_record hash32 VInt
