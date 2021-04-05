module Judge.Rule
  ( RuleT (..)
  , catchJustRule
  , subgoal
  , mismatch
  ) where

import Control.Monad (ap)
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Judge.Refine (RefineT (..), catchJustRefine)

-- | A @'RuleT'@ is a monad transformer for creating inference rules.
-- An inference rule is concerned with building extracted values.
-- It can also declare subgoals with 'subgoal'.
newtype RuleT j x e s m a = RuleT
  { unRuleT :: RefineT x a e s m j
  }

instance Functor m => Functor (RuleT j x e s m) where
  fmap f = RuleT . go . unRuleT where
    go p =
      case p of
        RefineSubgoal j k -> RefineSubgoal j (go . k)
        RefineEffect m -> RefineEffect (fmap go m)
        RefineState g -> RefineState (fmap go . g)
        RefineAlt t1 t2 -> RefineAlt (go t1) (go t2)
        RefineInterleave t1 t2 -> RefineInterleave (go t1) (go t2)
        RefineCommit t1 t2 -> RefineCommit (go t1) (go t2)
        RefineEmpty -> RefineEmpty
        RefineFailure e -> RefineFailure e
        RefineExtract a -> RefineExtract (f a)

instance Functor m => Applicative (RuleT j x e s m) where
  pure = RuleT . RefineExtract
  (<*>) = ap

instance Functor m => Monad (RuleT j x e s m) where
  return = pure
  RuleT p >>= f = RuleT (go p) where
    h = unRuleT . f
    go q =
      case q of
        RefineSubgoal j k -> RefineSubgoal j (go . k)
        RefineEffect m -> RefineEffect (fmap go m)
        RefineState g -> RefineState (fmap go . g)
        RefineAlt p1 p2 -> RefineAlt (go p1) (go p2)
        RefineInterleave p1 p2 -> RefineInterleave (go p1) (go p2)
        RefineCommit p1 p2 -> RefineCommit (go p1) (go p2)
        RefineEmpty -> RefineEmpty
        RefineFailure e -> RefineFailure e
        RefineExtract a -> h a

instance Monad m => MonadState s (RuleT jdg ext err s m) where
  state f = RuleT (RefineState (\s -> let (a, s') = f s in (s', RefineExtract a)))

catchJustRule :: Functor m => (e -> Maybe b) -> RuleT j x e s m a -> (b -> RuleT j x e s m a) -> RuleT j x e s m a
catchJustRule f r handle = RuleT (catchJustRefine f (unRuleT r) (unRuleT . handle))

instance Monad m => MonadError e (RuleT j x e s m) where
  throwError = RuleT . RefineFailure
  catchError = catchJustRule Just

instance MonadTrans (RuleT j x e s) where
  lift = RuleT . RefineEffect . fmap RefineExtract

-- | Declare the given judgement @j@ a subgoal of the proof
subgoal :: j -> RuleT j x e s m x
subgoal j = RuleT (RefineSubgoal j RefineExtract)

-- TODO not sure if ok
mismatch :: RuleT j x e s m x
mismatch = RuleT RefineEmpty
