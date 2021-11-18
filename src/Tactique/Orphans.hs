{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tactique.Orphans
  (
  ) where

import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans.Free (FreeT (..))

instance Functor f => MFunctor (FreeT f) where
  hoist trans = FreeT . trans . fmap (fmap (hoist trans)) . runFreeT
