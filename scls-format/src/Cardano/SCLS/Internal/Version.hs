module Cardano.SCLS.Internal.Version (
  Version (..),
  packVersion,
) where

import Data.Word

-- | Version of the SCLS format.
data Version
  = V1
  deriving (Show, Eq, Ord)

packVersion :: Version -> Word32
packVersion V1 = 1
