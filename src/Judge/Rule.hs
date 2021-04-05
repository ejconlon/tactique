module Judge.Rule
  ( RuleF (..)
  , RuleT (..)
  , Rule
  , subgoal
  , mismatch
  ) where

import Control.Monad.Identity (Identity)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free (MonadFree (..))
import Judge.Monads (SuspT)

data RuleF j x a =
    RuleSubgoal !j !(x -> a)
  | RuleMismatch !(x -> a)
  deriving (Functor)

newtype RuleT j x s e m a = RuleT
  { unRuleT :: SuspT (RuleF j x) s e m a
  } deriving (
    Functor, Applicative, Monad,
    MonadState s, MonadError e)

type Rule j x s e a = RuleT j x s e Identity a

instance Monad m => MonadFree (RuleF j x) (RuleT j x s e m) where
  wrap = RuleT . wrap . fmap unRuleT

instance MonadTrans (RuleT j x s e) where
  lift = RuleT . lift

instance MFunctor (RuleT j x s e) where
  hoist trans = RuleT . hoist trans . unRuleT

subgoal :: Monad m => j -> RuleT j x s e m x
subgoal j = wrap (RuleSubgoal j pure)

mismatch :: Monad m => RuleT j x s e m x
mismatch = wrap (RuleMismatch pure)
