module Judge.Rule
  ( RuleF (..)
  , RuleT (..)
  , Rule
  , subgoal
  , mismatch
  , runRuleT
  ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free (FreeF (..), MonadFree (..))
import Data.Sequence (Seq (..))
import Judge.Holes (HoleM, MonadHole (..), fromHole)
import Judge.Monads (SuspT, runSuspT)

data RuleF j x a =
    RuleSubgoal !j !(x -> a)
  | RuleMismatch !(x -> a)
  deriving (Functor)

newtype RuleT h j x s e m a = RuleT
  { unRuleT :: SuspT (RuleF j x) s e m a
  } deriving (
    Functor, Applicative, Monad,
    MonadState s, MonadError e)

type Rule h j x s e = RuleT h j x s e (HoleM h x)

instance Monad m => MonadFree (RuleF j x) (RuleT h j x s e m) where
  wrap = RuleT . wrap . fmap unRuleT

instance MonadTrans (RuleT h j x s e) where
  lift = RuleT . lift

instance MFunctor (RuleT h j x s e) where
  hoist trans = RuleT . hoist trans . unRuleT

subgoal :: Monad m => j -> RuleT h j x s e m x
subgoal j = wrap (RuleSubgoal j pure)

mismatch :: Monad m => RuleT h j x s e m x
mismatch = wrap (RuleMismatch pure)

runRuleT :: MonadHole h x m => RuleT h j x s e m a -> s -> m (Maybe (Either e (a, s, Seq (h, j))))
runRuleT = go Empty . unRuleT where
  go !goals f s =  do
    eas <- runSuspT f s
    case eas of
      Left e -> pure (Just (Left e))
      Right (fy, s') ->
        case fy of
          Pure a -> pure (Just (Right (a, s', goals)))
          Free y ->
            case y of
              RuleSubgoal g k -> do
                h <- newHole
                let x = fromHole h
                go (goals :|> (h, g)) (k x) s'
              RuleMismatch _ -> pure Nothing
