module Judge.Tactic
  ( TacticT
  , Tactic
  , goal
  , tactic
  , refine
  , applying
  , interleaving
  , committing
  , trying
  , repeating
  , choosing
  , progress
  , gather
  , ensure
  , rule
  , search
  , awaitSearch
  , simpleSearch
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (MonadState (..), StateT (..), modify)
import Data.Sequence (Seq)
import Judge.Internal.Lists (unfoldSeq)
import Judge.Refine (RefineT (..), proofs, subgoals)
import Judge.Rule (RuleT (..), subgoal)
import ListT (ListT)
import qualified ListT

newtype TacticT j x e s m a = TacticT { runTacticT :: StateT j (RefineT x x e s m) a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError e)

type Tactic j x e s a = TacticT j x e s Identity a

-- | Get the current goal
goal :: Functor m => TacticT j x e s m j
goal = TacticT get

-- | Helper function for producing a tactic.
tactic :: (j -> RefineT x x e s m (a, j)) -> TacticT j x e s m a
tactic = TacticT . StateT

-- |  Helper function for deconstructing a tactic.
refine :: TacticT j x e s m a -> j -> RefineT x x e s m (a, j)
refine = runStateT . runTacticT

-- | Create a tactic that applies each of the tactics in the list to one subgoal.
--
-- When the number of subgoals is greater than the number of provided tactics,
-- the identity tactic is applied to the remainder. When the number of subgoals is
-- less than the number of provided tactics, the remaining tactics are ignored.
applying :: Functor m => TacticT j x e s m a -> [TacticT j x e s m a] -> TacticT j x e s m a
applying t ts = tactic (subgoals (fmap (\t' (_, j') -> refine t' j') ts) . refine t)

-- | @interleaving t1 t2@ will interleave the execution of @t1@ and @t2@. This is useful if @t1@ will
-- produce an infinite number of extracts, as we will still run @t2@. This is contrasted with
-- @t1 <|> t2@, which will not ever consider @t2@ if @t1@ produces an infinite number of extracts.
interleaving :: TacticT j x e s m a -> TacticT j x e s m a -> TacticT j x e s m a
interleaving t1 t2 = tactic (\j -> RefineInterleave (refine t1 j) (refine t2 j))

-- | @commit t1 t2@ will run @t1@, and then only run @t2@ if @t1@ failed to produce any extracts.
committing :: TacticT jdg ext err s m a -> TacticT jdg ext err s m a -> TacticT jdg ext err s m a
committing t1 t2 = tactic $ \j -> RefineCommit (refine t1 j) (refine t2 j)

-- | Tries to run a tactic, backtracking on failure
trying :: Monad m => TacticT j x e s m () -> TacticT j x e s m ()
trying t = t <|> pure ()

-- | Runs a tactic repeatedly until it fails
repeating :: Monad m => TacticT j x e s m () -> TacticT j x e s m ()
repeating t = trying (t *> repeating t)

-- | @choice ts@ will run all of the tactics in the list against the current subgoals,
-- and interleave their extracts in a manner similar to 'interleaving'.
choosing :: (Foldable f, Monad m) => f (TacticT jdg ext err s m a) -> TacticT jdg ext err s m a
choosing = foldr interleaving empty
-- TODO works with asum (foldr (<|>) empty) but not interleave...

-- | @progress eq err t@ applies the tactic @t@, and checks to see if the
-- resulting subgoals are all equal to the initial goal by using @eq@. If they
-- are, it throws @err@.
progress :: Monad m => (j -> j -> Bool) -> e -> TacticT j x e s m a -> TacticT j x e s m a
progress eq err t = do
  j <- goal
  a <- t
  j' <- goal
  if j `eq` j' then pure a else throwError err

-- | @gather t f@ runs the tactic @t@, then runs @f@ with all of the generated subgoals to determine
-- the next tactic to run.
gather :: Monad m => m x -> TacticT j x e s m a -> (Seq (a, j) -> TacticT j x e s m a) -> TacticT j x e s m a
gather hole t f = tactic $ \j -> do
  s <- get
  let res = proofs hole s (refine t j)
      tacRes = fmap (either (throwError . fst) (\(_, _, jdgs) -> refine (f jdgs) j)) res
      -- TODO I don't think we need to consume the whole list of proofs. It forces
      -- effects when constructing Refines we may not use. Maybe we actually need to embed ListT
      finalRes = ListT.fold (\a b -> pure (a <|> b)) empty tacRes
  RefineEffect finalRes

-- | @pruning t f@ runs the tactic @t@, and then applies a predicate to all of the generated subgoals.
pruning :: Monad m => m x -> TacticT j x e s m () -> (Seq j -> Maybe e) -> TacticT j x e s m ()
pruning hole t p = gather hole t (maybe t throwError . p . fmap snd)

-- | @filterT p f t@ runs the tactic @t@, and applies a predicate to the state after the execution of @t@. We also run
-- a "cleanup" function @f@. Note that the predicate is applied to the state _before_ the cleanup function is run.
ensure :: Monad m => (s -> Maybe e) -> (s -> s) -> TacticT j x e s m () -> TacticT j x e s m ()
ensure p f t = check *> t where
  -- NOTE It may seem backwards to run check _before_ t, but we
  -- need to do the predicate check after the subgoal has been resolved,
  -- and not on every generated subgoal.
  check = rule $ \j -> do
    x <- subgoal j
    s <- get
    modify f
    case p s of
      Just e -> throwError e
      Nothing -> pure x

-- | Turn an inference rule into a tactic.
rule :: Monad m => (j -> RuleT j x e s m x) -> TacticT j x e s m ()
rule r = tactic (fmap ((),) . unRuleT . r)

-- | Runs a tactic, producing a list of possible extracts, along with a list of unsolved subgoals.
search :: Monad m => m x -> TacticT j x e s m () -> j -> s -> ListT m (Either (e, s) (x, s, Seq j))
search h t j s = proofs h s (fmap snd (refine t j))

awaitSearch :: Monad m => m x -> TacticT j x e s m () -> j -> s -> m (Seq (Either (e, s) (x, s, Seq j)))
awaitSearch h t j = unfoldSeq . search h t j

simpleSearch :: x -> Tactic j x e s () -> j -> s -> Seq (Either (e, s) (x, s, Seq j))
simpleSearch h t j = runIdentity . awaitSearch (pure h) t j
