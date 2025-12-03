{-# LANGUAGE DataKinds #-}

-- | Helper type that tells that the data is versioned by the namespace.
module Cardano.SCLS.Versioned (
  Versioned (..),
) where

import GHC.Generics
import GHC.TypeLits

newtype Versioned (ns :: Symbol) a = Versioned {unVer :: a}
  deriving (Eq, Generic, Ord, Bounded, Functor, Show)

instance Applicative (Versioned ns) where
  pure = Versioned
  (Versioned f) <*> (Versioned x) = Versioned (f x)
