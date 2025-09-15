{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Cardano.SCLS.Internal.Record.Internal.Class (
  IsFrameRecord (..),
  SomeRecord (..),
) where

import Data.Binary -- TODO: I'd like to switch to the `mempack` library
import Data.Typeable
import GHC.TypeLits

-- TODO: Update the CIP reference

{- | Class for the values in the frame record. Such class allows
to work only with intersting types of the records leaving other
around as required by CIP.

`t` should correspond to the record type code as defined in CIP-???
-}
class (KnownNat t) => IsFrameRecord t a | a -> t where
  decodeRecordContents :: Get a
  encodeRecordContents :: a -> Put

{- | Existential wrapper for any record that has an instance of
`IsFrameRecord`. It's useful for passing records around without
knowing their exact type.
-}
data SomeRecord = forall t a. (Typeable a, IsFrameRecord t a) => SomeRecord a
