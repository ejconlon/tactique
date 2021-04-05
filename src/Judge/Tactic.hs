module Judge.Tactic
  ( TacticT (..)
  , Tactic
  , goal
  , rule
  ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, void)
import Control.Monad.Except (MonadError)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (MonadReader, ReaderT (..))
-- import Control.Monad.State (MonadState (..), StateT (..), gets, modify')
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Sequence (Seq)
import Judge.Rule (RuleF (..), RuleT (..))
import Judge.Search (SearchF (..), SearchT (..))
import Judge.Monads (transSuspT)
import ListT

newtype TacticT j x s e m a = TacticT
  { unTacticT :: ReaderT j (SearchT j x s e m) a
  } deriving (
    Functor, Applicative, Monad,
    Alternative, MonadPlus, MonadError e, MonadReader j)

type Tactic j x s e a = TacticT j x s e Identity a

instance MonadTrans (TacticT j x s e) where
  lift = TacticT . lift . lift

instance MFunctor (TacticT j x s e) where
  hoist trans = TacticT . hoist (hoist trans) . unTacticT

-- runTacticT :: TacticT j x s e m a -> j -> SearchT j x s e m a
-- runTacticT = runReaderT . unTacticT

runTacticT :: Monad m => m x -> TacticT j x e s m a -> j -> s -> ListT m (Either e (a, s, Seq j))
runTacticT = undefined

goal :: Monad m => TacticT j x s e m j
goal = TacticT (ReaderT pure)

natTransRuleSearch :: RuleF j x a -> SearchF j x a
natTransRuleSearch r =
  case r of
    RuleSubgoal j k -> SearchSubgoal j k
    RuleMismatch _ -> SearchEmpty

transRuleSearch :: Monad m => RuleT j x s e m a -> SearchT j x s e m a
transRuleSearch = SearchT . transSuspT natTransRuleSearch . unRuleT

rule :: Monad m => (j -> RuleT j x s e m a) -> TacticT j x s e m a
rule f = TacticT (ReaderT (transRuleSearch . f))
