{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.SCLS.CBOR.Canonical.Decoder where

import Cardano.SCLS.Internal.Version (Version)
import Codec.CBOR.ByteArray qualified as BA
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.Read qualified as CBOR.Read
import Control.Exception (Exception)
import Data.Array.Byte qualified as Prim
import Data.ByteString (ByteString)
import Data.ByteString.Short qualified as SBS
import Data.Int
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Word
import GHC.Generics (Generic)

newtype Versioned (v :: Version) a = Versioned {unVer :: a}
  deriving (Eq, Generic, Ord, Bounded, Functor)

class FromCanonicalCBOR (v :: Version) a where
  fromCanonicalCBOR :: Decoder s (Versioned v a)

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data DecodingError
  = DecodingErrorDeserialiseFailure T.Text CBOR.Read.DeserialiseFailure
  deriving (Eq, Show)

instance Exception DecodingError

--------------------------------------------------------------------------------
-- Primitive types
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v () where
  fromCanonicalCBOR = Versioned @v <$> D.decodeNull

instance FromCanonicalCBOR v Bool where
  fromCanonicalCBOR = Versioned @v <$> D.decodeBool

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v Integer where
  fromCanonicalCBOR = Versioned @v <$> D.decodeInteger

instance FromCanonicalCBOR v Word where
  fromCanonicalCBOR = Versioned @v <$> D.decodeWord

instance FromCanonicalCBOR v Word8 where
  fromCanonicalCBOR = Versioned @v <$> D.decodeWord8

instance FromCanonicalCBOR v Word16 where
  fromCanonicalCBOR = Versioned @v <$> D.decodeWord16

instance FromCanonicalCBOR v Word32 where
  fromCanonicalCBOR = Versioned @v <$> D.decodeWord32

instance FromCanonicalCBOR v Word64 where
  fromCanonicalCBOR = Versioned @v <$> D.decodeWord64

instance FromCanonicalCBOR v Int where
  fromCanonicalCBOR = Versioned @v <$> D.decodeInt

instance FromCanonicalCBOR v Int32 where
  fromCanonicalCBOR = Versioned @v <$> D.decodeInt32

instance FromCanonicalCBOR v Int64 where
  fromCanonicalCBOR = Versioned @v <$> D.decodeInt64

--------------------------------------------------------------------------------
-- Bytes
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v ByteString where
  fromCanonicalCBOR = Versioned @v <$> D.decodeBytes

instance FromCanonicalCBOR v SBS.ShortByteString where
  fromCanonicalCBOR = do
    BA.BA (Prim.ByteArray ba) <- D.decodeByteArray
    pure $ Versioned @v $ SBS.SBS ba

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

instance
  (FromCanonicalCBOR v a, FromCanonicalCBOR v b) =>
  FromCanonicalCBOR v (a, b)
  where
  fromCanonicalCBOR = do
    D.decodeListLenOf 2
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b)

instance
  (FromCanonicalCBOR v a, FromCanonicalCBOR v b, FromCanonicalCBOR v c) =>
  FromCanonicalCBOR v (a, b, c)
  where
  fromCanonicalCBOR = do
    D.decodeListLenOf 2
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c)

instance
  ( FromCanonicalCBOR v a
  , FromCanonicalCBOR v b
  , FromCanonicalCBOR v c
  , FromCanonicalCBOR v d
  ) =>
  FromCanonicalCBOR v (a, b, c, d)
  where
  fromCanonicalCBOR = do
    D.decodeListLenOf 2
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    Versioned d <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c, d)

instance
  ( FromCanonicalCBOR v a
  , FromCanonicalCBOR v b
  , FromCanonicalCBOR v c
  , FromCanonicalCBOR v d
  , FromCanonicalCBOR v e
  ) =>
  FromCanonicalCBOR v (a, b, c, d, e)
  where
  fromCanonicalCBOR = do
    D.decodeListLenOf 2
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    Versioned d <- fromCanonicalCBOR @v
    Versioned e <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c, d, e)

instance
  ( FromCanonicalCBOR v a
  , FromCanonicalCBOR v b
  , FromCanonicalCBOR v c
  , FromCanonicalCBOR v d
  , FromCanonicalCBOR v e
  , FromCanonicalCBOR v f
  ) =>
  FromCanonicalCBOR v (a, b, c, d, e, f)
  where
  fromCanonicalCBOR = do
    D.decodeListLenOf 2
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    Versioned d <- fromCanonicalCBOR @v
    Versioned e <- fromCanonicalCBOR @v
    Versioned f <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c, d, e, f)

instance
  ( FromCanonicalCBOR v a
  , FromCanonicalCBOR v b
  , FromCanonicalCBOR v c
  , FromCanonicalCBOR v d
  , FromCanonicalCBOR v e
  , FromCanonicalCBOR v f
  , FromCanonicalCBOR v g
  ) =>
  FromCanonicalCBOR v (a, b, c, d, e, f, g)
  where
  fromCanonicalCBOR = do
    D.decodeListLenOf 2
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    Versioned d <- fromCanonicalCBOR @v
    Versioned e <- fromCanonicalCBOR @v
    Versioned f <- fromCanonicalCBOR @v
    Versioned g <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c, d, e, f, g)

instance
  ( FromCanonicalCBOR v a
  , FromCanonicalCBOR v b
  , FromCanonicalCBOR v c
  , FromCanonicalCBOR v d
  , FromCanonicalCBOR v e
  , FromCanonicalCBOR v f
  , FromCanonicalCBOR v g
  , FromCanonicalCBOR v h
  ) =>
  FromCanonicalCBOR v (a, b, c, d, e, f, g, h)
  where
  fromCanonicalCBOR = do
    D.decodeListLenOf 2
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    Versioned d <- fromCanonicalCBOR @v
    Versioned e <- fromCanonicalCBOR @v
    Versioned f <- fromCanonicalCBOR @v
    Versioned g <- fromCanonicalCBOR @v
    Versioned h <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c, d, e, f, g, h)

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

-- | We always encode lists with the indefinite length encoding.
instance (FromCanonicalCBOR v a) => FromCanonicalCBOR v [a] where
  fromCanonicalCBOR = do
    D.decodeListLenIndef
    D.decodeSequenceLenIndef
      (\acc (Versioned x) -> x : acc)
      []
      (Versioned @v . reverse)
      (fromCanonicalCBOR @v)

instance (FromCanonicalCBOR v a) => FromCanonicalCBOR v (Seq.Seq a) where
  fromCanonicalCBOR = fmap Seq.fromList <$> fromCanonicalCBOR

--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

-- | We always encode maps with the indefinite length encoding.
instance
  (FromCanonicalCBOR v k, FromCanonicalCBOR v val) =>
  FromCanonicalCBOR v (Map.Map k val)
  where
  fromCanonicalCBOR = do
    D.decodeMapLenIndef
    asList <-
      D.decodeSequenceLenIndef
        (\acc x -> x : acc)
        []
        id
        decodeEntry
    -- Note that the ordering is reversed since we prepend to the list
    pure $ Versioned @v $ Map.fromDistinctDescList asList
   where
    decodeEntry = do
      Versioned a <- fromCanonicalCBOR @v
      Versioned b <- fromCanonicalCBOR @v
      return (a, b)
