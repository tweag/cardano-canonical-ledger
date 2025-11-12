module Cardano.SCLS.Internal.Version (
  Version (..),
  packVersion,
  unpackVersion,
) where

import Data.MemPack (MemPack (..))
import Data.MemPack.ByteOrdered (BigEndian (..), packWord32beM, unpackBigEndianM)
import Data.Word (Word32)

-- | Version of the SCLS format.
data Version
  = V1
  deriving (Show, Eq, Ord)

packVersion :: Version -> (BigEndian Word32)
packVersion V1 = BigEndian 1

unpackVersion :: (BigEndian Word32) -> Version
unpackVersion (BigEndian 1) = V1
unpackVersion (BigEndian n) = error $ "Unknown version: " <> show n

instance MemPack Version where
  packedByteCount _ = 4

  packM V1 = packWord32beM 1

  unpackM = do
    v :: Word32 <- unpackBigEndianM
    case v of
      1 -> pure V1
      n -> fail $ "Unknown version: " <> show n
