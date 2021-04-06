{-# LANGUAGE Rank2Types #-}

module Judge.Free.Rule
  ( RuleT
  , Rule
  , subgoal
  , mismatch
  ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity)
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Free (FreeF (..), FreeT (..))
import Judge.Alt.Internal (NatTrans, StatePair (..), idTrans)

data RuleF j x s e a =
    RuleErrorF !e
  | RuleStateF !(s -> StatePair s a)
  | RuleSubgoalF !j !(x -> a)
  | RuleMismatchF !(x -> a)
  deriving (Functor)

newtype RuleT j x s e m a = RuleT { unRuleT :: FreeT (RuleF j x s e) m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

type Rule j x s e a = RuleT j x s e Identity a

runRuleT :: (MonadError e n, MonadState s n) => NatTrans m n -> n x -> (j -> n x) -> RuleT j x s e m a -> n a
runRuleT trans onMismatch onSubgoal = go . unRuleT where
  go ft = do
    f <- trans (runFreeT ft)
    case f of
      Pure a -> pure a
      Free w ->
        case w of
          RuleErrorF e -> throwError e
          RuleStateF y -> do
            s <- get
            let StatePair s' a = y s
            put s'
            go a
          RuleSubgoalF j k -> do
            x <- onSubgoal j
            go (k x)
          RuleMismatchF k -> do
            x <- onMismatch
            go (k x)

runRule :: (MonadError e n, MonadState s n) => n x -> (j -> n x) -> Rule j x s e a -> n a
runRule = runRuleT idTrans

instance Monad m => MonadState s (RuleT j x s e m) where
  state f = RuleT (FreeT (pure (Free (RuleStateF (\s -> let (a, !s') = f s in StatePair s' (pure a))))))

instance Monad m => MonadError e (RuleT j x s e m) where
  throwError e = RuleT (FreeT (pure (Free (RuleErrorF e))))
  catchError r h = RuleT (go (unRuleT r)) where
    g = unRuleT . h
    go ft = FreeT $ do
      f <- runFreeT ft
      case f of
        Pure _ -> pure f
        Free w ->
          case w of
            RuleErrorF e -> runFreeT (g e)
            RuleStateF y -> pure (Free (RuleStateF (fmap go . y)))
            RuleSubgoalF j k -> pure (Free (RuleSubgoalF j (go . k)))
            RuleMismatchF k -> pure (Free (RuleMismatchF (go . k)))

subgoal :: Monad m => j -> RuleT j x s e m x
subgoal j = RuleT (FreeT (pure (Free (RuleSubgoalF j pure))))

mismatch :: Monad m => RuleT j x s e m x
mismatch = RuleT (FreeT (pure (Free (RuleMismatchF pure))))
