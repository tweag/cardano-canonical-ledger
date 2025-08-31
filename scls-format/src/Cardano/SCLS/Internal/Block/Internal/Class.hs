{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Cardano.SCLS.Internal.Block.Internal.Class
  ( IsFrameBlock(..)
  ) where

import Data.Binary -- TODO: I'd like to switch to the `mempack` library

-- | Class for the values in the frame block. Such class allows
-- to work only with intersting types of the blocks leaving other
-- around as required by CIP.
class IsFrameBlock t a | a -> t where
    decodeBlockContents :: Get a
    encodeBlockContents :: a -> Put