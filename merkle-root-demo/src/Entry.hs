module Entry
  ( Entry (Entry),
    serialize,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Test.QuickCheck (Arbitrary (arbitrary), elements, listOf1)

data Entry = Entry
  { namespace :: String,
    key :: String,
    value :: String
  }
  deriving (Show, Eq)

serialize :: Entry -> B.ByteString
serialize e =
  B.concat . map C.pack $ [namespace e, key e, value e]

instance Arbitrary Entry where
  arbitrary = do
    ns <- listOf1 $ elements (['a' .. 'z'])
    k <- listOf1 $ elements ['a' .. 'z']
    v <- listOf1 $ elements ['a' .. 'z']
    return $ Entry ns k v
