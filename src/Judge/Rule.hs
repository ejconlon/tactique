module Judge.Rule
  ( RuleF (..)
  , RuleT (..)
  , ruleSubgoal
  , ruleMismatch
  , runRuleT
  ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State.Strict (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free (FreeF (..), MonadFree (..))
import Data.Sequence (Seq (..))
import Judge.Holes (MonadHole (..), fromHole)
import Judge.Monads (SuspT, runSuspT)

data RuleF j x a =
    RuleSubgoal !j !(x -> a)
  | RuleMismatch !(x -> a)
  deriving (Functor)

newtype RuleT j x s e m a = RuleT
  { unRuleT :: SuspT (RuleF j x) s e m a
  } deriving (
    Functor, Applicative, Monad,
    MonadState s, MonadError e)

instance Monad m => MonadFree (RuleF j x) (RuleT j x s e m) where
  wrap = RuleT . wrap . fmap unRuleT

instance MonadTrans (RuleT j x s e) where
  lift = RuleT . lift

instance MFunctor (RuleT j x s e) where
  hoist trans = RuleT . hoist trans . unRuleT

instance MonadIO m => MonadIO (RuleT j x s e m) where
  liftIO = lift . liftIO

ruleSubgoal :: Monad m => j -> RuleT j x s e m x
ruleSubgoal j = wrap (RuleSubgoal j pure)

ruleMismatch :: Monad m => RuleT j x s e m x
ruleMismatch = wrap (RuleMismatch pure)

runRuleT :: MonadHole h x m => RuleT j x s e m a -> s -> m (Maybe (Either e (a, s, Seq (h, j))))
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
