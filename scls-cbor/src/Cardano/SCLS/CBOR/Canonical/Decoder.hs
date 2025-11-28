{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.SCLS.CBOR.Canonical.Decoder where

import Cardano.SCLS.Versioned
import Codec.CBOR.ByteArray qualified as BA
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.Read qualified as CBOR.Read
import Control.Exception (Exception)
import Data.Array.Byte qualified as Prim
import Data.ByteString (ByteString)
import Data.ByteString.Short qualified as SBS
import Data.Coerce
import Data.Int
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word
import GHC.TypeLits

class FromCanonicalCBOR (v :: Symbol) a where
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
  fromCanonicalCBOR = Versioned @v <$> D.decodeIntegerCanonical

instance FromCanonicalCBOR v Word where
  fromCanonicalCBOR = Versioned @v <$> D.decodeWordCanonical

instance FromCanonicalCBOR v Word8 where
  fromCanonicalCBOR = Versioned @v <$> D.decodeWord8Canonical

instance FromCanonicalCBOR v Word16 where
  fromCanonicalCBOR = Versioned @v <$> D.decodeWord16Canonical

instance FromCanonicalCBOR v Word32 where
  fromCanonicalCBOR = Versioned @v <$> D.decodeWord32Canonical

instance FromCanonicalCBOR v Word64 where
  fromCanonicalCBOR = Versioned @v <$> D.decodeWord64Canonical

instance FromCanonicalCBOR v Int where
  fromCanonicalCBOR = Versioned @v <$> D.decodeIntCanonical

instance FromCanonicalCBOR v Int32 where
  fromCanonicalCBOR = Versioned @v <$> D.decodeInt32Canonical

instance FromCanonicalCBOR v Int64 where
  fromCanonicalCBOR = Versioned @v <$> D.decodeInt64Canonical

--------------------------------------------------------------------------------
-- Bytes
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v ByteString where
  fromCanonicalCBOR = Versioned @v <$> D.decodeBytesCanonical

instance FromCanonicalCBOR v SBS.ShortByteString where
  fromCanonicalCBOR = do
    BA.BA (Prim.ByteArray ba) <- D.decodeByteArrayCanonical
    pure $ Versioned @v $ SBS.SBS ba

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v T.Text where
  fromCanonicalCBOR = Versioned @v <$> D.decodeStringCanonical

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

instance
  (FromCanonicalCBOR v a, FromCanonicalCBOR v b) =>
  FromCanonicalCBOR v (a, b)
  where
  fromCanonicalCBOR = do
    D.decodeListLenCanonicalOf 2
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b)

instance
  (FromCanonicalCBOR v a, FromCanonicalCBOR v b, FromCanonicalCBOR v c) =>
  FromCanonicalCBOR v (a, b, c)
  where
  fromCanonicalCBOR = do
    D.decodeListLenCanonicalOf 2
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
    D.decodeListLenCanonicalOf 2
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
    D.decodeListLenCanonicalOf 2
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
    D.decodeListLenCanonicalOf 2
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
    D.decodeListLenCanonicalOf 2
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
    D.decodeListLenCanonicalOf 2
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

-- | We always encode lists with the definite length encoding.
instance (FromCanonicalCBOR v a) => FromCanonicalCBOR v [a] where
  fromCanonicalCBOR = do
    len <- D.decodeListLenCanonical
    D.decodeSequenceLenN
      (\acc (Versioned x) -> x : acc)
      []
      (Versioned @v . reverse)
      len
      (fromCanonicalCBOR @v)

instance (FromCanonicalCBOR v a) => FromCanonicalCBOR v (Seq.Seq a) where
  fromCanonicalCBOR = fmap Seq.fromList <$> fromCanonicalCBOR

--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

-- | We always encode maps with the definite length encoding. This does not check canonical order of keys.
instance
  (Ord k, FromCanonicalCBOR v k, FromCanonicalCBOR v val) =>
  FromCanonicalCBOR v (Map.Map k val)
  where
  fromCanonicalCBOR = do
    len <- D.decodeMapLenCanonical
    asList <-
      D.decodeSequenceLenN
        (\acc x -> x : acc)
        []
        id
        len
        decodeEntry
    -- Use Map.fromList because encoding uses encoded key byte-order,
    -- which may not match deserialized order
    pure $ Versioned @v $ Map.fromList asList
   where
    decodeEntry = do
      Versioned a <- fromCanonicalCBOR @v
      Versioned b <- fromCanonicalCBOR @v
      return (a, b)

--------------------------------------------------------------------------------
-- Set
--------------------------------------------------------------------------------

-- | We always encode sets with the definite length encoding.
instance
  (Ord a, FromCanonicalCBOR v a) =>
  FromCanonicalCBOR v (Set.Set a)
  where
  fromCanonicalCBOR = do
    258 <- D.decodeTagCanonical
    n <- D.decodeListLenCanonical
    D.decodeSequenceLenN
      (\acc x -> x : acc)
      []
      (coerce Set.fromList :: [Versioned v a] -> Versioned v (Set.Set a))
      n
      (fromCanonicalCBOR @v)
