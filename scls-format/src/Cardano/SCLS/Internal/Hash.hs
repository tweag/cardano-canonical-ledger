module Cardano.SCLS.Internal.Hash (
  Digest (..),
  digest,
  hashDigestSize,
  digestFromByteString,
) where

import Crypto.Hash qualified as CH
import Data.Binary (Binary, get, put)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteArray qualified as BA

type HashAlgorithm = CH.Blake2b_224

hashDigestSize :: Int
hashDigestSize = CH.hashDigestSize (undefined :: HashAlgorithm)

digestFromByteString :: (BA.ByteArrayAccess ba) => ba -> Maybe Digest
digestFromByteString = fmap Digest . CH.digestFromByteString

newtype Digest = Digest (CH.Digest HashAlgorithm)
  deriving (Eq, Ord, Show, Read)

instance Binary Digest where
  put (Digest h) = putByteString $ BA.convert h

  get = do
    bs <- getByteString hashDigestSize
    case CH.digestFromByteString bs of
      Just h -> pure $ Digest h
      Nothing -> fail "Invalid digest"

digest :: (BA.ByteArrayAccess ba) => ba -> Digest
digest = Digest . CH.hash
