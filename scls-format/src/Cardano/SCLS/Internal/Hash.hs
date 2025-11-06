module Cardano.SCLS.Internal.Hash (
  Digest (..),
  digest,
  hashDigestSize,
  digestFromByteString,
) where

import Crypto.Hash qualified as CH
import Data.ByteArray qualified as BA
import Data.MemPack

type HashAlgorithm = CH.Blake2b_224

hashDigestSize :: Int
hashDigestSize = CH.hashDigestSize (undefined :: HashAlgorithm)

digestFromByteString :: (BA.ByteArrayAccess ba) => ba -> Maybe Digest
digestFromByteString = fmap Digest . CH.digestFromByteString

newtype Digest = Digest (CH.Digest HashAlgorithm)
  deriving (Eq, Ord, Show, Read)

instance BA.ByteArrayAccess Digest where
  length (Digest h) = BA.length h
  withByteArray (Digest h) f = BA.withByteArray h f

instance MemPack Digest where
  packedByteCount _ = hashDigestSize

  packM (Digest h) = packByteStringM $ BA.convert h

  unpackM = do
    bs <- unpackByteStringM hashDigestSize
    case digestFromByteString bs of
      Just d -> pure d
      Nothing -> fail "Invalid digest"

digest :: (BA.ByteArrayAccess ba) => ba -> Digest
digest = Digest . CH.hash
