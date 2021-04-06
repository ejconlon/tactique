{-# LANGUAGE Rank2Types #-}

module Judge.Alt.Rule
  ( RuleT (..)
  , Rule
  , subgoal
  , mismatch
  ) where

import Control.Monad (ap)
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.Identity (Identity)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State (MonadState (..), StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Judge.Alt.Internal (StatePair (..))

data RuleT j x s e m a =
    RuleError !e
  | RuleState !(s -> StatePair s (RuleT j x s e m a))
  | RuleEffect (m (RuleT j x s e m a))
  | RuleSubgoal !j !(x -> RuleT j x s e m a)
  | RuleMismatch !(x -> RuleT j x s e m a)
  | RulePure !a
  deriving (Functor)

-- data Query j = QuerySubgoal !j | QueryMismatch deriving (Eq, Show)

-- newtype Oracle j e s m a = Oracle { unOracle :: Query j -> ExceptT e (StateT s) m a }
--   deriving (Functor, Applicative, Monad)

-- data ST j s = ST !j !s deriving (Eq, Show)

-- data OnRule j x s e a n r = OnRule
--   { onRuleError :: (e -> n ())
--   , onRuleGetState :: n s
--   , onRuleSetState :: (s -> n ())
--   , onRuleSubgoal :: !(j -> n x)
--   , onRulePure :: !(a -> n ())

-- class Monad n => MonadInterp j x s n | n -> j x where
--   interpSubgoal :: j -> s -> n x
--   interpMismatch :: s -> n x

-- newtype RuleInterpM j x s e n a = RuleInterpM { unRuleInterpM :: ExceptT e (StateT s n) a }
--   deriving (Functor, Applicative, Monad, MonadError e, MonadState s)

-- interpRule :: MonadInterp j x n => RuleT j x s e m a -> NatTrans m n -> RuleInterpM j x s e n a
-- interpRule r0 (NatTrans trans) = go r0 where
--   go r =
--     case r of
--       RuleError !e
--       RuleState !(s -> StatePair s (RuleT j x s e m a))
--       RuleEffect (m (RuleT j x s e m a))
--       RuleSubgoal !j !(x -> RuleT j x s e m a)
--       RuleMismatch onMis = do
--         x <- interpMismatch

--       RulePure val -> pure val


-- data T j x s e m a =
--     TError !e
--   | TEffect (m (T j x s e m a))
--   | TState (ST j s -> StatePair (ST j s) (T j x s e m a))
--   deriving (Functor)


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
