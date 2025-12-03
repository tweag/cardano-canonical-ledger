{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- | Specification for the namespaces keys.
module Cardano.SCLS.NamespaceKey (
  namespaceKeySize,
  NamespaceKeySize,
) where

import GHC.TypeLits (KnownNat, Nat, Symbol, fromSNat, pattern SNat)

{- | Maps a namespace (represented as a type-level string/Symbol) to its expected key size.

Instances for this type family should be created for each namespace/version.

For example:
@
type instance NamespaceKeySize "utxo/v0" = 34
@
-}
type family NamespaceKeySize (ns :: Symbol) :: Nat

-- | Get the namespace key size at runtime.
namespaceKeySize :: forall ns. (KnownNat (NamespaceKeySize ns)) => Int
namespaceKeySize =
  fromInteger $ fromSNat $ SNat @(NamespaceKeySize ns)
