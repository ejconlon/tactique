{-# OPTIONS_GHC -fno-warn-orphans #-}

module Judge.Alt.Orphans
  (
  )where

import Control.Applicative (empty)
import Control.Monad.Logic.Class (MonadLogic (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import ListT (ListT (..))
import qualified ListT

-- not needed (yet?)
instance MonadLogic Seq where
  msplit s =
    case s of
      Empty -> Seq.singleton Nothing
      a :<| as -> Seq.singleton (Just (a, as))

instance Monad m => MonadLogic (ListT m) where
  msplit = ListT . fmap (fmap go) . ListT.uncons where
    go (a, t) = (Just (a, t), empty)
