module Judge.Tactic
  ( TacticT (..)
  , Tactic
  , goal
  , rule
  ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Judge.Holes (MonadHole (..))
import Judge.Monads (transSuspT)
import Judge.Data.NEStack (NEStack)
import qualified Judge.Data.NEStack as NEStack
import Judge.Rule (RuleF (..), RuleT (..))
import Judge.Search (SearchF (..), SearchT (..), search)
import ListT (ListT (..))
import qualified ListT

newtype TacticT j x s e m a = TacticT
  { unTacticT :: ReaderT (NEStack j) (SearchT j x s e m) a
  } deriving (
    Functor, Applicative, Monad,
    Alternative, MonadPlus, MonadError e, MonadReader (NEStack j))

type Tactic j x s e a = TacticT j x s e Identity a

instance MonadTrans (TacticT j x s e) where
  lift = TacticT . lift . lift

instance MFunctor (TacticT j x s e) where
  hoist trans = TacticT . hoist (hoist trans) . unTacticT

candidates :: MonadHole h x m => TacticT j x s e m a -> NEStack j -> s -> ListT m (Either e (a, s, Seq (h, j)))
candidates t js = search (runReaderT (unTacticT t) js)

data Derivation h j x = Derivation
  { derivGoal :: !j
  , derivSoln :: !x
  , derivHoles :: !(Seq (h, Derivation h j x))
  } deriving (Eq, Show)

derive :: MonadHole h x m => TacticT j x s e m x -> j -> s -> ListT m (Either e (Derivation h j x))
derive t j0 s0 = ListT . go j0 (candidates t j s) where
  go j listt = do
    mex <- ListT.uncons listt
    case mex of
      Nothing -> pure Nothing
      Just (ex, nextListt) ->
        case ex of
          Left e -> pure (Just (Left e), nextListt)
          Right (x, s', js') ->
            if Seq.null js'
              then
              else undefined

goal :: Monad m => TacticT j x s e m j
goal = asks NEStack.peek

natTransRuleSearch :: RuleF j x a -> SearchF j x a
natTransRuleSearch r =
  case r of
    RuleSubgoal j k -> SearchSubgoal j k
    RuleMismatch _ -> SearchEmpty

transRuleSearch :: Monad m => RuleT j x s e m a -> SearchT j x s e m a
transRuleSearch = SearchT . transSuspT natTransRuleSearch . unRuleT

rule :: Monad m => (j -> RuleT j x s e m a) -> TacticT j x s e m a
rule f = undefined -- TacticT (ReaderT (transRuleSearch . f))
