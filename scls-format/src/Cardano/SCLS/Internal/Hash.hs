module Cardano.SCLS.Internal.Hash (
  Digest (..),
  digest,
) where

import Crypto.Hash qualified as CH
import Data.Binary (Binary, get, put)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteArray qualified as BA

type HashAlgorithm = CH.Blake2b_224

newtype Digest = Digest (CH.Digest HashAlgorithm)
  deriving (Eq, Ord, Show, Read)

instance Binary Digest where
  put (Digest h) = do
    putWord32be $ fromIntegral $ CH.hashDigestSize (undefined :: HashAlgorithm)
    putByteString $ BA.convert $ h

  get = do
    len <- getWord32be
    bs <- getByteString (fromIntegral len)
    case CH.digestFromByteString bs of
      Just h -> pure $ Digest h
      Nothing -> fail "Invalid digest"

digest :: (BA.ByteArrayAccess ba) => ba -> Digest
digest = Digest . CH.hash
