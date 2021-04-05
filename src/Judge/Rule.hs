module Judge.Rule
  ( RuleT (..)
  , Rule
  , subgoal
  , mismatch
  ) where

import Control.Monad (ap)
import Control.Monad.Identity (Identity)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Judge.Internal (StatePair (..))

data RuleT j x s e m a =
    RuleError !e
  | RuleState !(s -> StatePair s (RuleT j x s e m a))
  | RuleEffect (m (RuleT j x s e m a))
  | RuleSubgoal !j !(x -> RuleT j x s e m a)
  | RuleMismatch !(x -> RuleT j x s e m a)
  | RulePure !a
  deriving (Functor)

type Rule j x s e a = RuleT j x s e Identity a

instance Functor m => Applicative (RuleT j x s e m) where
  pure = RulePure
  (<*>) = ap

instance Functor m => Monad (RuleT j x s e m) where
  return = pure
  r0 >>= f = go r0 where
    go r =
      case r of
        RuleError err -> RuleError err
        RuleState onState -> RuleState (fmap go . onState)
        RuleEffect eff -> RuleEffect (fmap go eff)
        RuleSubgoal jdg onSub -> RuleSubgoal jdg (go . onSub)
        RuleMismatch onMis -> RuleMismatch (go . onMis)
        RulePure val -> f val

instance MonadTrans (RuleT j x s e) where
  lift = RuleEffect . fmap RulePure

instance MFunctor (RuleT j x s e) where
  hoist trans = go where
    go r =
      case r of
        RuleError err -> RuleError err
        RuleState onState -> RuleState (fmap go . onState)
        RuleEffect eff -> RuleEffect (trans (fmap go eff))
        RuleSubgoal jdg onSub -> RuleSubgoal jdg (go . onSub)
        RuleMismatch onMis -> RuleMismatch (go . onMis)
        RulePure val -> RulePure val

instance Monad m => MonadState s (RuleT j x s e m) where
  state f = RuleState (\s -> let (a, !s') = f s in StatePair s' (RulePure a))

instance Monad m => MonadError e (RuleT j x s e m) where
  throwError = RuleError
  catchError r0 h = go r0 where
    go r =
      case r of
        RuleError err -> h err
        RuleState onState -> RuleState (fmap go . onState)
        RuleEffect eff -> RuleEffect (fmap go eff)
        RuleSubgoal j onSub -> RuleSubgoal j (go . onSub)
        RuleMismatch onMis -> RuleMismatch (go . onMis)
        RulePure _ -> r

subgoal :: j -> RuleT j x s e m x
subgoal j = RuleSubgoal j RulePure

mismatch :: RuleT j x s e m x
mismatch = RuleMismatch RulePure
