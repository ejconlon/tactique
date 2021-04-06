module Judge.Refine
  ( RefineT (..)
  , Refine
  , catchJustRefine
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
import Judge.Internal.Lists (guardRight, unfoldSeq)
import Judge.Internal.Orphans ()
import ListT (ListT (..))
import qualified ListT

-- | 'RefineT' is a monad supporting all relevant proof script operations.
-- The parameters are as follows (with mnemonics):
-- @z@ - solution (Zolution)
-- @x@ - extract (eXtract)
-- @e@ - error (Error)
-- @s@ - state (State)
-- @m@ - monadic effect (Monad)
-- @a@ - goal/output (goAl/Anything)
data RefineT z x e s m a =
    -- | Yields a subgoal in a proof script. Continues with a solution.
    -- Many instances require that this solution type be the same as the
    RefineSubgoal a (z -> RefineT z x e s m a)
    -- | Embeds an effect in a proof script
  | RefineEffect (m (RefineT z x e s m a))
    -- | Threads state through the proof script
  | RefineState (s -> (s, RefineT z x e s m a))
    -- | Concatenates two proof scripts
  | RefineAlt (RefineT z x e s m a) (RefineT z x e s m a)
    -- | Interleaves two proof scripts fairly
  | RefineInterleave (RefineT z x e s m a) (RefineT z x e s m a)
    -- | Tries one proof script before the other
  | RefineCommit (RefineT z x e s m a) (RefineT z x e s m a)
    -- | Empty proof script
  | RefineEmpty
    -- | Failing proof script
  | RefineFailure e
    -- | Proof script returning an extracted value
  | RefineExtract x
  deriving (Functor)

type Refine z x e s a = RefineT z x e s Identity a

instance Functor m => Applicative (RefineT x x e s m) where
    pure a = RefineSubgoal a RefineExtract
    (<*>) = ap

mapExtracted
  :: Functor m
  => (x -> RefineT z x e s m a)
  -> RefineT z x e s m a
  -> RefineT z x e s m a
mapExtracted f = go where
  go p =
    case p of
      RefineSubgoal a k -> RefineSubgoal a (go . k)
      RefineEffect m -> RefineEffect (fmap go m)
      RefineState g -> RefineState (fmap go . g)
      RefineAlt p1 p2 -> RefineAlt (go p1) (go p2)
      RefineInterleave p1 p2 -> RefineInterleave (go p1) (go p2)
      RefineCommit p1 p2 -> RefineCommit (go p1) (go p2)
      RefineEmpty -> RefineEmpty
      RefineFailure e -> RefineFailure e
      RefineExtract x -> f x

instance Functor m => Monad (RefineT x x e s m) where
    return = pure
    p >>= f =
      case p of
        RefineSubgoal a k -> mapExtracted (k >=> f) (f a)
        RefineEffect m -> RefineEffect (fmap (>>= f) m)
        RefineState g -> RefineState (fmap (>>= f) . g)
        RefineAlt p1 p2 -> RefineAlt (p1 >>= f) (p2 >>= f)
        RefineInterleave p1 p2 -> RefineInterleave (p1 >>= f) (p2 >>= f)
        RefineCommit p1 p2 -> RefineCommit (p1 >>= f) (p2 >>= f)
        RefineFailure e -> RefineFailure e
        RefineEmpty -> RefineEmpty
        RefineExtract x -> RefineExtract x

instance Functor m => Alternative (RefineT x x e s m) where
  empty = RefineEmpty
  (<|>) = RefineAlt

instance Monad m => MonadPlus (RefineT x x e s m) where
    mzero = empty
    mplus = (<|>)

-- A catchJust for RefineT
catchJustRefine :: Functor m => (e -> Maybe b) -> RefineT z x e s m a -> (b -> RefineT z x e s m a) -> RefineT z x e s m a
catchJustRefine f p handle = go p where
  go q =
    case q of
      RefineSubgoal a k -> RefineSubgoal a (go . k)
      RefineEffect m -> RefineEffect (fmap go m)
      RefineState g -> RefineState (fmap go . g)
      RefineAlt p1 p2 -> RefineAlt (go p1) (go p2)
      RefineInterleave p1 p2 -> RefineInterleave (go p1) (go p2)
      RefineCommit p1 p2 -> RefineCommit (go p1) (go p2)
      RefineEmpty -> RefineEmpty
      RefineFailure e -> maybe q handle (f e)
      RefineExtract x -> RefineExtract x

instance Functor m => MonadError e (RefineT x x e s m) where
  throwError = RefineFailure
  catchError = catchJustRefine Just

instance Functor m => MonadState s (RefineT x x e s m) where
  state f = RefineState (\s -> let (a, s') = f s in (s', pure a))

instance MonadTrans (RefineT x x e s) where
  lift = RefineEffect . fmap pure

proofs :: Monad m => m x -> s -> RefineT x x e s m a -> ListT m (Either (e, s) (x, s, Seq a))
proofs hole = go Empty where
  go goals s (RefineSubgoal a k) = ListT $ do
    h <- hole
    let next = go (goals :|> a) s (k h)
    ListT.uncons next
  go goals s (RefineEffect m) = ListT $ do
    p <- m
    let next = go goals s p
    ListT.uncons next
  go goals s (RefineState g) = let (s', p) = g s in go goals s' p
  go goals s (RefineAlt p1 p2) = go goals s p1 <> go goals s p2
  go goals s (RefineInterleave p1 p2) = interleave (go goals s p1) (go goals s p2)
  go goals s (RefineCommit p1 p2) = ListT $ do
    mg1 <- guardRight (go goals s p1)
    let next = fromMaybe (go goals s p2) mg1
    ListT.uncons next
  go _ _ RefineEmpty = empty
  go _ s (RefineFailure e) = pure (Left (e, s))
  go goals s (RefineExtract x) = pure (Right (x, s, goals))

awaitProofs :: Monad m => m x -> s -> RefineT x x e s m a -> m (Seq (Either (e, s) (x, s, Seq a)))
awaitProofs hole st = unfoldSeq . proofs hole st

simpleProofs :: x -> s -> Refine x x e s a -> Seq (Either (e, s) (x, s, Seq a))
simpleProofs hole st = runIdentity . awaitProofs (pure hole) st

subgoals :: Functor m => [a -> RefineT x x e s m a] -> RefineT x x e s m a -> RefineT x x e s m a
subgoals fs p =
  case p of
    RefineSubgoal a k ->
      case fs of
        [] -> mapExtracted k (pure a)
        f:fs' -> mapExtracted (subgoals fs' . k) (f a)
    RefineEffect m -> RefineEffect (fmap (subgoals fs) m)
    RefineState s -> RefineState (fmap (subgoals fs) . s)
    RefineAlt p1 p2 -> RefineAlt (subgoals fs p1) (subgoals fs p2)
    RefineInterleave p1 p2 -> RefineInterleave (subgoals fs p1) (subgoals fs p2)
    RefineCommit p1 p2 -> RefineCommit (subgoals fs p1) (subgoals fs p2)
    RefineEmpty -> p
    RefineFailure _ -> p
    RefineExtract _ -> p
