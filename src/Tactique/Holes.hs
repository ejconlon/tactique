module Tactique.Holes
  ( HasHoles (..)
  , Cutout (..)
  , MonadHole (..)
  , HoleT (..)
  , runHoleT
  , HoleM
  , runHoleM
  ) where

import Control.Applicative (Const (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Sequence.NonEmpty as NESeq
import Tactique.Data.Validation (ValidT (..))

class HasHoles h x | x -> h where
  fromHole :: h -> x
  substHoles :: Applicative m => (h -> m x) -> x -> m x

  findHoles :: x -> Seq h
  findHoles = getConst . substHoles (Const . Seq.singleton)

  trySubstHoles :: Applicative m => (h -> m (Maybe x)) -> x -> m (Either (NESeq h) x)
  trySubstHoles fill = runValidT . substHoles (\h -> ValidT (fmap (maybe (Left (NESeq.singleton h)) Right) (fill h)))

data Cutout h x =
    CutoutHole !h
  | CutoutPiece !x
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasHoles h (Cutout h x) where
  fromHole = CutoutHole
  substHoles f c =
    case c of
      CutoutHole h -> f h
      CutoutPiece _ -> pure c

class (HasHoles h x, Monad m) => MonadHole h x m | m -> h x where
  newHole :: m h

newtype HoleT h x m a = HoleT
  { unHoleT :: StateT h m a
  } deriving (Functor, Applicative, Monad, MonadState h)

type HoleM h x = HoleT h x Identity

instance MonadTrans (HoleT h x) where
  lift = HoleT . lift

instance (HasHoles h x, Enum h, Monad m) => MonadHole h x (HoleT h x m) where
  newHole = state (\h -> (h, succ h))

instance MonadIO m => MonadIO (HoleT h x m) where
  liftIO = lift . liftIO

runHoleT :: HoleT h x m a -> h -> m (a, h)
runHoleT = runStateT . unHoleT

runHoleM :: HoleM h x a -> h -> (a, h)
runHoleM m = runIdentity . runHoleT m
