module Judge.Alt.Tactic
  ( TacticT
  , Tactic
  , goal
  , rule
  ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (MonadState (..), StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Sequence (Seq)
import Judge.Alt.Internal (StatePair (..))
import Judge.Alt.Proof (ProofT (..))
import Judge.Alt.Result (Result)
import Judge.Alt.Rule (RuleT (..))
import ListT (ListT)

newtype TacticT j x s e m a = TacticT { unTacticT :: StateT j (ProofT x s e m) a }
  deriving (Functor, Applicative, Monad, MonadError e)

type Tactic j x s e a = TacticT j x s e Identity a

instance MonadTrans (TacticT j x s e) where
  lift = TacticT . lift . lift

instance Monad m => MonadState s (TacticT j x s e m) where
  state f = TacticT (StateT (\j -> ProofState (\s -> let (a, !s') = f s in StatePair s' (pure (a, j)))))

goal :: Monad m => TacticT j x e s m j
goal = TacticT get

tactic :: (j -> ProofT x s e m (a, j)) -> TacticT j x s e m a
tactic = TacticT . StateT

proof :: TacticT j x s e m a -> j -> ProofT x s e m (a, j)
proof = runStateT . unTacticT

convertRule :: Functor m => RuleT j x s e m x -> j -> ProofT x s e m ((), j)
convertRule r jdg =
  case r of
    RuleError err -> ProofError err
    RuleState onState -> ProofState $ \s ->
      let StatePair s' r' = onState s
      in StatePair s' (convertRule r' jdg)
    RuleEffect eff -> ProofEffect (fmap (`convertRule` jdg) eff)
    RuleSubgoal subJdg onSub -> ProofSubgoal ((), subJdg) ((`convertRule` subJdg) . onSub)
    RuleMismatch onMis -> ProofSubgoal ((), jdg) ((`convertRule` jdg) . onMis)
    RulePure _ -> ProofSubgoal ((), jdg) ProofExtract

rule :: Functor m => (j -> RuleT j x s e m x) -> TacticT j x s e m ()
rule onJdg = tactic (\j -> convertRule (onJdg j) j)

search :: Monad m => m x -> TacticT j x s e m () -> j -> s -> ListT m (Result x s e j)
search = undefined

-- simpleSearch :: Monad n => x -> Tactic j x s e () -> j -> s -> Seq (Result x s e j)
-- simpleSearch = search
