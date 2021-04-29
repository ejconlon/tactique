{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tactique.Orphans
  (
  ) where

import Control.Applicative (empty)
import Control.Monad.Logic.Class (MonadLogic (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans.Free (FreeT (..))
import ListT (ListT (..))
import qualified ListT

instance Monad m => MonadLogic (ListT m) where
  msplit = ListT . fmap (fmap go) . ListT.uncons where
    go (a, t) = (Just (a, t), empty)

instance Functor f => MFunctor (FreeT f) where
  hoist trans = FreeT . trans . fmap (fmap (hoist trans)) . runFreeT
