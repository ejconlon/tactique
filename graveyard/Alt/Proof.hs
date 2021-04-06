module Judge.Alt.Proof
  ( ProofT (..)
  , Proof
  , proofs
  , awaitProofs
  , simpleProofs
  , subgoals
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), ap, (>=>))
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Logic.Class (MonadLogic (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import Judge.Alt.Internal (StatePair (..))
import Judge.Alt.Lists (guardSuccess, unfoldSeq)
import Judge.Alt.Orphans ()
import Judge.Alt.Result (Result (..))
import ListT (ListT (..))
import qualified ListT

-- | 'ProofT' is a monad supporting all relevant proof script operations.
-- The parameters are as follows (with mnemonics):
-- @x@ - extract (eXtract)
-- @e@ - error (Error)
-- @s@ - state (State)
-- @m@ - monadic effect (Monad)
-- @a@ - goal/output (goAl/Anything)
data ProofT x s e m a =
    -- | Yields a subgoal in a proof script. Continues with a solution.
    ProofSubgoal !a !(x -> ProofT x s e m a)
    -- | Embeds an effect in a proof script
  | ProofEffect (m (ProofT x s e m a))
    -- | Threads state through the proof script
  | ProofState !(s -> StatePair s (ProofT x s e m a))
    -- | Concatenates two proof scripts
  | ProofAlt (ProofT x s e m a) (ProofT x s e m a)
    -- | Interleaves two proof scripts fairly
  | ProofInterleave (ProofT x s e m a) (ProofT x s e m a)
    -- | Tries one proof script before the other
  | ProofCommit (ProofT x s e m a) (ProofT x s e m a)
    -- | Empty proof script
  | ProofEmpty
    -- | Failing proof script
  | ProofError !e
    -- | Proof script returning an extracted value
  | ProofExtract !x
  deriving (Functor)

type Proof x s e a = ProofT x s e Identity a

instance Functor m => Applicative (ProofT x s e m) where
    pure a = ProofSubgoal a ProofExtract
    (<*>) = ap

mapExtracted :: Functor m => (x -> ProofT x s e m a) -> ProofT x s e m a -> ProofT x s e m a
mapExtracted f = go where
  go p =
    case p of
      ProofSubgoal goal onExt -> ProofSubgoal goal (go . onExt)
      ProofEffect eff -> ProofEffect (fmap go eff)
      ProofState onState -> ProofState (fmap go . onState)
      ProofAlt p1 p2 -> ProofAlt (go p1) (go p2)
      ProofInterleave p1 p2 -> ProofInterleave (go p1) (go p2)
      ProofCommit p1 p2 -> ProofCommit (go p1) (go p2)
      ProofEmpty -> ProofEmpty
      ProofError err -> ProofError err
      ProofExtract ext -> f ext

instance Functor m => Monad (ProofT x s e m) where
  return = pure
  p >>= f =
    case p of
      ProofSubgoal goal onExt -> mapExtracted (onExt >=> f) (f goal)
      ProofEffect eff -> ProofEffect (fmap (>>= f) eff)
      ProofState onState -> ProofState (fmap (>>= f) . onState)
      ProofAlt p1 p2 -> ProofAlt (p1 >>= f) (p2 >>= f)
      ProofInterleave p1 p2 -> ProofInterleave (p1 >>= f) (p2 >>= f)
      ProofCommit p1 p2 -> ProofCommit (p1 >>= f) (p2 >>= f)
      ProofError err -> ProofError err
      ProofEmpty -> ProofEmpty
      ProofExtract ext -> ProofExtract ext

instance Functor m => Alternative (ProofT x s e m) where
  empty = ProofEmpty
  (<|>) = ProofAlt

instance Monad m => MonadPlus (ProofT x s e m) where
  mzero = empty
  mplus = (<|>)

instance Functor m => MonadError e (ProofT x s e m) where
  throwError = ProofError
  catchError p0 h = go p0 where
    go p =
      case p of
        ProofSubgoal goal onExt -> ProofSubgoal goal (go . onExt)
        ProofEffect eff -> ProofEffect (fmap go eff)
        ProofState onState -> ProofState (fmap go . onState)
        ProofAlt p1 p2 -> ProofAlt (go p1) (go p2)
        ProofInterleave p1 p2 -> ProofInterleave (go p1) (go p2)
        ProofCommit p1 p2 -> ProofCommit (go p1) (go p2)
        ProofEmpty -> ProofEmpty
        ProofError err -> h err
        ProofExtract ext -> ProofExtract ext

instance Functor m => MonadState s (ProofT x s e m) where
  state f = ProofState (\s -> let (a, !s') = f s in StatePair s' (pure a))

instance MonadTrans (ProofT x s e) where
  lift = ProofEffect . fmap pure

proofs :: Monad m => m x -> s -> ProofT x s e m a -> ListT m (Result x s e a)
proofs hole = go Empty where
  go goals s (ProofSubgoal goal onExt) = ListT $ do
    h <- hole
    let next = go (goals :|> goal) s (onExt h)
    ListT.uncons next
  go goals s (ProofEffect eff) = ListT $ do
    p <- eff
    let next = go goals s p
    ListT.uncons next
  go goals s (ProofState onState) = let StatePair s' p = onState s in go goals s' p
  go goals s (ProofAlt p1 p2) = go goals s p1 <> go goals s p2
  go goals s (ProofInterleave p1 p2) = interleave (go goals s p1) (go goals s p2)
  go goals s (ProofCommit p1 p2) = ListT $ do
    mg1 <- guardSuccess (go goals s p1)
    let next = fromMaybe (go goals s p2) mg1
    ListT.uncons next
  go _ _ ProofEmpty = empty
  go _ s (ProofError err) = pure (ResultError s err)
  go goals s (ProofExtract ext) = pure (ResultSuccess s ext goals)

awaitProofs :: Monad m => m x -> s -> ProofT x s e m a -> m (Seq (Result x s e a))
awaitProofs hole st = unfoldSeq . proofs hole st

simpleProofs :: x -> s -> Proof x s e a -> Seq (Result x s e a)
simpleProofs hole st = runIdentity . awaitProofs (pure hole) st

subgoals :: Functor m => [a -> ProofT x s e m a] -> ProofT x s e m a -> ProofT x s e m a
subgoals fs p =
  case p of
    ProofSubgoal a k ->
      case fs of
        [] -> mapExtracted k (pure a)
        f:fs' -> mapExtracted (subgoals fs' . k) (f a)
    ProofEffect m -> ProofEffect (fmap (subgoals fs) m)
    ProofState s -> ProofState (fmap (subgoals fs) . s)
    ProofAlt p1 p2 -> ProofAlt (subgoals fs p1) (subgoals fs p2)
    ProofInterleave p1 p2 -> ProofInterleave (subgoals fs p1) (subgoals fs p2)
    ProofCommit p1 p2 -> ProofCommit (subgoals fs p1) (subgoals fs p2)
    ProofEmpty -> p
    ProofError _ -> p
    ProofExtract _ -> p
