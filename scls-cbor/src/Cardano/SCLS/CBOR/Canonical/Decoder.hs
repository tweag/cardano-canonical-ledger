{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeListLenCanonicalOf,
  decodeListLenCanonical,
  decodeMapLenCanonical,
  decodeMapLenCanonicalOf,
  decodeTagCanonical,
  decodeTextOfCanonical,
  decodeWordCanonicalOf,
  decodeWord8Canonical,
  peekTokenType,
) where

import Cardano.SCLS.NamespaceCodec (CanonicalDecoder (CanonicalDecoder, unCanonicalDecoder))
import Cardano.SCLS.Versioned
import Codec.CBOR.ByteArray qualified as BA
import Codec.CBOR.Decoding (TokenType)
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
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word
import GHC.TypeLits

class FromCanonicalCBOR (v :: Symbol) a where
  fromCanonicalCBOR :: CanonicalDecoder s (Versioned v a)

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
  fromCanonicalCBOR = CanonicalDecoder $ Versioned @v <$> D.decodeNull

instance FromCanonicalCBOR v Bool where
  fromCanonicalCBOR = CanonicalDecoder $ Versioned @v <$> D.decodeBool

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v Integer where
  fromCanonicalCBOR = CanonicalDecoder $ Versioned @v <$> D.decodeIntegerCanonical

instance FromCanonicalCBOR v Word where
  fromCanonicalCBOR = CanonicalDecoder $ Versioned @v <$> D.decodeWordCanonical

instance FromCanonicalCBOR v Word8 where
  fromCanonicalCBOR = CanonicalDecoder $ Versioned @v <$> D.decodeWord8Canonical

instance FromCanonicalCBOR v Word16 where
  fromCanonicalCBOR = CanonicalDecoder $ Versioned @v <$> D.decodeWord16Canonical

instance FromCanonicalCBOR v Word32 where
  fromCanonicalCBOR = CanonicalDecoder $ Versioned @v <$> D.decodeWord32Canonical

instance FromCanonicalCBOR v Word64 where
  fromCanonicalCBOR = CanonicalDecoder $ Versioned @v <$> D.decodeWord64Canonical

instance FromCanonicalCBOR v Int where
  fromCanonicalCBOR = CanonicalDecoder $ Versioned @v <$> D.decodeIntCanonical

instance FromCanonicalCBOR v Int32 where
  fromCanonicalCBOR = CanonicalDecoder $ Versioned @v <$> D.decodeInt32Canonical

instance FromCanonicalCBOR v Int64 where
  fromCanonicalCBOR = CanonicalDecoder $ Versioned @v <$> D.decodeInt64Canonical

--------------------------------------------------------------------------------
-- Bytes
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v ByteString where
  fromCanonicalCBOR = CanonicalDecoder $ Versioned @v <$> D.decodeBytesCanonical

instance FromCanonicalCBOR v SBS.ShortByteString where
  fromCanonicalCBOR = do
    BA.BA (Prim.ByteArray ba) <- CanonicalDecoder D.decodeByteArrayCanonical
    pure $ Versioned @v $ SBS.SBS ba

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v T.Text where
  fromCanonicalCBOR = Versioned @v <$> CanonicalDecoder D.decodeStringCanonical

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

instance
  (FromCanonicalCBOR v a, FromCanonicalCBOR v b) =>
  FromCanonicalCBOR v (a, b)
  where
  fromCanonicalCBOR = do
    CanonicalDecoder $ D.decodeListLenCanonicalOf 2
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b)

instance
  (FromCanonicalCBOR v a, FromCanonicalCBOR v b, FromCanonicalCBOR v c) =>
  FromCanonicalCBOR v (a, b, c)
  where
  fromCanonicalCBOR = do
    CanonicalDecoder $ D.decodeListLenCanonicalOf 2
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
    CanonicalDecoder $ D.decodeListLenCanonicalOf 2
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
    CanonicalDecoder $ D.decodeListLenCanonicalOf 2
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
    CanonicalDecoder $ D.decodeListLenCanonicalOf 2
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
    CanonicalDecoder $ D.decodeListLenCanonicalOf 2
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
    CanonicalDecoder $ D.decodeListLenCanonicalOf 2
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
    len <- CanonicalDecoder $ D.decodeListLenCanonical
    CanonicalDecoder $
      D.decodeSequenceLenN
        (\acc (Versioned x) -> x : acc)
        []
        (Versioned @v . reverse)
        len
        (unCanonicalDecoder $ fromCanonicalCBOR @v)

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
    len <- CanonicalDecoder $ D.decodeMapLenCanonical
    asList <-
      CanonicalDecoder $
        D.decodeSequenceLenN
          (\acc x -> x : acc)
          []
          id
          len
          (unCanonicalDecoder decodeEntry)
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
    258 <- CanonicalDecoder D.decodeTagCanonical
    n <- CanonicalDecoder D.decodeListLenCanonical
    CanonicalDecoder $
      D.decodeSequenceLenN
        (\acc x -> x : acc)
        []
        (coerce Set.fromList :: [Versioned v a] -> Versioned v (Set.Set a))
        n
        (unCanonicalDecoder fromCanonicalCBOR)

decodeTextOfCanonical :: Text -> CanonicalDecoder s ()
decodeTextOfCanonical t = do
  t' <- CanonicalDecoder D.decodeStringCanonical
  if t == t'
    then
      pure ()
    else
      fail $ "expected string " ++ show t

decodeMapLenCanonical :: CanonicalDecoder s Int
decodeMapLenCanonical =
  CanonicalDecoder D.decodeMapLenCanonical

decodeMapLenCanonicalOf :: Int -> CanonicalDecoder s ()
decodeMapLenCanonicalOf len = do
  len' <- CanonicalDecoder D.decodeMapLenCanonical
  if len == len'
    then
      pure ()
    else
      fail $ "expected map of length " ++ show len

decodeListLenCanonical :: CanonicalDecoder s Int
decodeListLenCanonical =
  CanonicalDecoder D.decodeListLenCanonical

decodeListLenCanonicalOf :: Int -> CanonicalDecoder s ()
decodeListLenCanonicalOf =
  CanonicalDecoder . D.decodeListLenCanonicalOf

decodeWordCanonicalOf :: Word -> CanonicalDecoder s ()
decodeWordCanonicalOf =
  CanonicalDecoder . D.decodeWordCanonicalOf

decodeWord8Canonical :: CanonicalDecoder s Word8
decodeWord8Canonical =
  CanonicalDecoder D.decodeWord8Canonical

peekTokenType :: CanonicalDecoder s TokenType
peekTokenType =
  CanonicalDecoder D.peekTokenType

decodeTagCanonical :: CanonicalDecoder s Word
decodeTagCanonical =
  CanonicalDecoder D.decodeTagCanonical
