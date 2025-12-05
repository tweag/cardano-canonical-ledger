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
import Text.Heredoc (str)

record_entry :: Rule
record_entry =
  comment
    [str| The key for the entry is epoch number (8 bytes) |]
    $ "record_entry" =:= committee

committee :: Rule
committee =
  "committee"
    =:= (mp [0 <+ asKey credential ==> committee_authorization])

committee_authorization :: Rule
committee_authorization =
  "committee_authorization"
    =:= arr [0, a credential]
    / arr [1, a (anchor / VNil)]
