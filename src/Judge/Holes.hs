module Judge.Holes
  ( HasHole (..)
  , Cutout (..)
  , fillHoles
  , tryFillHoles
  , MonadHole (..)
  , HoleT (..)
  , runHoleT
  , HoleM
  , runHoleM
  ) where

import Control.Monad.Identity (Identity (..))
import Control.Monad.State (MonadState (..), StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Judge.Validation (ValidT (..))

class HasHole h x | x -> h where
  fromHole :: h -> x
  matchHole :: x -> Maybe h

data Cutout h x =
    CutoutHole !h
  | CutoutPiece !x
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasHole h (Cutout h x) where
  fromHole = CutoutHole
  matchHole x =
    case x of
      CutoutHole h -> Just h
      CutoutPiece _ -> Nothing

fillHoles :: (HasHole h x, Applicative m, Traversable f) => (h -> m x) -> f x -> m (f x)
fillHoles fill = traverse (\x -> maybe (pure x) fill (matchHole x))

tryFillHoles :: (HasHole h x, Applicative m, Traversable f) => (h -> m (Maybe x)) -> f x -> m (Either (Seq h) (f x))
tryFillHoles fill = runValidT . fillHoles (\h -> ValidT (fmap (maybe (Left (Seq.singleton h)) Right) (fill h)))

class (HasHole h x, Monad m) => MonadHole h x m | m -> h x where
  newHole :: m h

newtype HoleT h x m a = HoleT
  { unHoleT :: StateT h m a
  } deriving (Functor, Applicative, Monad, MonadState h)

type HoleM h x a = HoleT h x Identity a

instance MonadTrans (HoleT h x) where
  lift = HoleT . lift

instance (HasHole h x, Enum h, Monad m) => MonadHole h x (HoleT h x m) where
  newHole = state (\h -> (h, succ h))

runHoleT :: HoleT h x m a -> h -> m (a, h)
runHoleT = runStateT . unHoleT

runHoleM :: HoleM h x a -> h -> (a, h)
runHoleM m = runIdentity . runHoleT m
