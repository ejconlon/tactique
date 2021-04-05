{-# OPTIONS_GHC -fno-warn-orphans #-}

module Judge.Internal.Orphans
  (
  ) where

import Control.Applicative (empty)
import Control.Monad.Logic.Class (MonadLogic (..))
import ListT (ListT (..))
import qualified ListT

instance Monad m => MonadLogic (ListT m) where
  msplit = ListT . fmap (fmap go) . ListT.uncons where
    go (a, t) = (Just (a, t), empty)
