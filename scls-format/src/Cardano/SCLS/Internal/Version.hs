{-# LANGUAGE DerivingVia #-}

module Cardano.SCLS.Internal.Version (
  Version (..),
  packVersion,
  unpackVersion,
) where

import Cardano.Types.ByteOrdered (BigEndian (..))
import Data.Word

-- | Version of the SCLS format.
data Version
  = V1
  deriving (Show, Eq, Ord)

packVersion :: Version -> (BigEndian Word32)
packVersion V1 = BigEndian 1

unpackVersion :: (BigEndian Word32) -> Version
unpackVersion (BigEndian 1) = V1
unpackVersion (BigEndian n) = error $ "Unknown version: " <> show n
