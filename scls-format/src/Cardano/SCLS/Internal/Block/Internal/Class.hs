{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Cardano.SCLS.Internal.Block.Internal.Class
  ( IsFrameBlock(..)
  ) where

import Data.Binary -- TODO: I'd like to switch to the `mempack` library
import GHC.TypeLits

-- TODO: Update the CIP reference
-- | Class for the values in the frame block. Such class allows
-- to work only with intersting types of the blocks leaving other
-- around as required by CIP.
--
-- `t` should correspond to the record type code as defined in CIP-???
class KnownNat t => IsFrameBlock t a | a -> t where
    decodeBlockContents :: Get a
    encodeBlockContents :: a -> Put
