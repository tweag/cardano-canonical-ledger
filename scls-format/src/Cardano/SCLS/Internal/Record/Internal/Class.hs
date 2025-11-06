{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Cardano.SCLS.Internal.Record.Internal.Class (
  IsFrameRecord (..),
  SomeRecord (..),
  mkRecordType,
) where

import Data.MemPack (Pack, Unpack)
import Data.MemPack.Buffer (Buffer)
import Data.Typeable
import Data.Word (Word32, Word8)
import GHC.TypeLits

-- TODO: Update the CIP reference

{- | Class for the values in the frame record. Such class allows
to work only with intersting types of the records leaving other
around as required by CIP.

`t` should correspond to the record type code as defined in CIP-???
-}
class (KnownNat t) => IsFrameRecord t a | a -> t where
  frameRecordSize :: a -> Int
  decodeRecordContents :: (Buffer b) => Word32 -> Unpack b a
  encodeRecordContents :: a -> Pack s ()

{- | Existential wrapper for any record that has an instance of
`IsFrameRecord`. It's useful for passing records around without
knowing their exact type.
-}
data SomeRecord = forall t a. (Typeable a, IsFrameRecord t a) => SomeRecord a

-- | Create a record type code from the record type.
mkRecordType :: forall a t. (IsFrameRecord t a) => Word8
mkRecordType = fromIntegral $ natVal (Proxy @t)
