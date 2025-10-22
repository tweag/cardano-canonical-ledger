{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Implementation of the state machine that fills current chunk in memory.

It manages proper filling of the buffers and emitting the values when
the next item can't be added.

Current implementation expects the incoming values in already sorted order.

Implementation is done in the way so it would be possible to use it with
any existing stream and effect system as long as they could carry a state.
-}
module Cardano.SCLS.Internal.Serializer.MetadataBuilder.InMemory (
  mkMachine,
  B.Command (..),
  BuilderMachine,
  MetadataItem (..),
  B.interpretCommand,
) where

import Cardano.SCLS.Internal.Serializer.Builder.InMemory qualified as B
import Data.Primitive.ByteArray

data MetadataItem = MetadataItem
  { metadataItemData :: ByteArray
  , metadataItemEntriesCount :: Int
  }

instance B.BuilderItem MetadataItem where
  type Parameters MetadataItem = ()
  bEncodeEntry _ = id
  bItemData = metadataItemData
  bItemEntriesCount = metadataItemEntriesCount
  bMkItem _ data_ count = MetadataItem{metadataItemData = data_, metadataItemEntriesCount = count}

type BuilderMachine = (B.BuilderMachine MetadataItem)

mkMachine :: Int -> IO (B.BuilderMachine MetadataItem)
mkMachine = flip B.mkMachine ()
