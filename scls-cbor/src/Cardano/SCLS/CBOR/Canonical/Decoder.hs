{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeListLenCanonicalOf,
  decodeListLenCanonical,
  decodeMapLenCanonical,
  decodeMapLenCanonicalOf,
  decodeTagCanonical,
  decodeTextCanonicalOf,
  decodeWordCanonicalOf,
  decodeWord8Canonical,
  peekTokenType,
  decodeSequenceLenNCanonical,
) where

import Cardano.SCLS.CBOR.Canonical (CanonicalDecoder (unCanonicalDecoder), unsafeToCanonicalDecoder)
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
  fromCanonicalCBOR = unsafeToCanonicalDecoder $ Versioned @v <$> D.decodeNull

instance FromCanonicalCBOR v Bool where
  fromCanonicalCBOR = unsafeToCanonicalDecoder $ Versioned @v <$> D.decodeBool

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v Integer where
  fromCanonicalCBOR = unsafeToCanonicalDecoder $ Versioned @v <$> D.decodeIntegerCanonical

instance FromCanonicalCBOR v Word where
  fromCanonicalCBOR = unsafeToCanonicalDecoder $ Versioned @v <$> D.decodeWordCanonical

instance FromCanonicalCBOR v Word8 where
  fromCanonicalCBOR = unsafeToCanonicalDecoder $ Versioned @v <$> D.decodeWord8Canonical

instance FromCanonicalCBOR v Word16 where
  fromCanonicalCBOR = unsafeToCanonicalDecoder $ Versioned @v <$> D.decodeWord16Canonical

instance FromCanonicalCBOR v Word32 where
  fromCanonicalCBOR = unsafeToCanonicalDecoder $ Versioned @v <$> D.decodeWord32Canonical

instance FromCanonicalCBOR v Word64 where
  fromCanonicalCBOR = unsafeToCanonicalDecoder $ Versioned @v <$> D.decodeWord64Canonical

instance FromCanonicalCBOR v Int where
  fromCanonicalCBOR = unsafeToCanonicalDecoder $ Versioned @v <$> D.decodeIntCanonical

instance FromCanonicalCBOR v Int32 where
  fromCanonicalCBOR = unsafeToCanonicalDecoder $ Versioned @v <$> D.decodeInt32Canonical

instance FromCanonicalCBOR v Int64 where
  fromCanonicalCBOR = unsafeToCanonicalDecoder $ Versioned @v <$> D.decodeInt64Canonical

--------------------------------------------------------------------------------
-- Bytes
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v ByteString where
  fromCanonicalCBOR = unsafeToCanonicalDecoder $ Versioned @v <$> D.decodeBytesCanonical

instance FromCanonicalCBOR v SBS.ShortByteString where
  fromCanonicalCBOR = do
    BA.BA (Prim.ByteArray ba) <- unsafeToCanonicalDecoder D.decodeByteArrayCanonical
    pure $ Versioned @v $ SBS.SBS ba

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v T.Text where
  fromCanonicalCBOR = Versioned @v <$> unsafeToCanonicalDecoder D.decodeStringCanonical

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

instance
  (FromCanonicalCBOR v a, FromCanonicalCBOR v b) =>
  FromCanonicalCBOR v (a, b)
  where
  fromCanonicalCBOR = do
    unsafeToCanonicalDecoder $ D.decodeListLenCanonicalOf 2
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b)

instance
  (FromCanonicalCBOR v a, FromCanonicalCBOR v b, FromCanonicalCBOR v c) =>
  FromCanonicalCBOR v (a, b, c)
  where
  fromCanonicalCBOR = do
    unsafeToCanonicalDecoder $ D.decodeListLenCanonicalOf 3
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
    unsafeToCanonicalDecoder $ D.decodeListLenCanonicalOf 4
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
    unsafeToCanonicalDecoder $ D.decodeListLenCanonicalOf 5
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
    unsafeToCanonicalDecoder $ D.decodeListLenCanonicalOf 6
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
    unsafeToCanonicalDecoder $ D.decodeListLenCanonicalOf 7
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
    unsafeToCanonicalDecoder $ D.decodeListLenCanonicalOf 8
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
    len <- unsafeToCanonicalDecoder $ D.decodeListLenCanonical
    decodeSequenceLenNCanonical
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
    len <- unsafeToCanonicalDecoder $ D.decodeMapLenCanonical
    asList <-
      decodeSequenceLenNCanonical
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
    258 <- decodeTagCanonical
    n <- decodeListLenCanonical
    decodeSequenceLenNCanonical
      (\acc x -> x : acc)
      []
      (coerce Set.fromList :: [Versioned v a] -> Versioned v (Set.Set a))
      n
      fromCanonicalCBOR

decodeTextCanonicalOf :: Text -> CanonicalDecoder s ()
decodeTextCanonicalOf t = do
  t' <- unsafeToCanonicalDecoder D.decodeStringCanonical
  if t == t'
    then
      pure ()
    else
      fail $ "expected string " ++ show t

decodeMapLenCanonical :: CanonicalDecoder s Int
decodeMapLenCanonical =
  unsafeToCanonicalDecoder D.decodeMapLenCanonical

decodeMapLenCanonicalOf :: Int -> CanonicalDecoder s ()
decodeMapLenCanonicalOf len = do
  len' <- decodeMapLenCanonical
  if len == len'
    then
      pure ()
    else
      fail $ "expected map of length " ++ show len

decodeListLenCanonical :: CanonicalDecoder s Int
decodeListLenCanonical =
  unsafeToCanonicalDecoder D.decodeListLenCanonical

decodeListLenCanonicalOf :: Int -> CanonicalDecoder s ()
decodeListLenCanonicalOf =
  unsafeToCanonicalDecoder . D.decodeListLenCanonicalOf

decodeWordCanonicalOf :: Word -> CanonicalDecoder s ()
decodeWordCanonicalOf =
  unsafeToCanonicalDecoder . D.decodeWordCanonicalOf

decodeWord8Canonical :: CanonicalDecoder s Word8
decodeWord8Canonical =
  unsafeToCanonicalDecoder D.decodeWord8Canonical

peekTokenType :: CanonicalDecoder s TokenType
peekTokenType =
  unsafeToCanonicalDecoder D.peekTokenType

decodeTagCanonical :: CanonicalDecoder s Word
decodeTagCanonical =
  unsafeToCanonicalDecoder D.decodeTagCanonical

decodeSequenceLenNCanonical :: (r -> a -> r) -> r -> (r -> r') -> Int -> CanonicalDecoder s a -> CanonicalDecoder s r'
decodeSequenceLenNCanonical f i f' n d =
  unsafeToCanonicalDecoder
    $ D.decodeSequenceLenN
      f
      i
      f'
      n
    $ unCanonicalDecoder d
