module Judge.Tactic
  ( TacticT (..)
  , Tactic
  , goal
  , rule
  , trying
  , repeating
  , interleaving
  , choosing
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.Logic.Class (interleave)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State (MonadState (..), StateT (..), gets, modify')
import Control.Monad.Trans (MonadTrans (..))
import Data.Sequence (Seq (..))
import Judge.Data.TreeZ (breadthTreeZ, depthTreeZ, firstTreeZ, stateTreeZ)
import Judge.Derivation (DerivEnd, DerivError, DerivTree, DerivTreeZ, Evaluated (..), derivGoal, derivZGoal, startDeriv)
import Judge.Holes (HoleM, MonadHole (..))
import Judge.Monads (BaseT (..), runBaseT)
import Judge.Orphans ()
import Judge.Rule (RuleT (..), runRuleT)
import ListT (ListT (..))
import qualified ListT

data TraversalOrder = TraversalOrderDepthFirst | TraversalOrderBreadthFirst deriving (Eq, Show)

data TacGoalState h j x =
    TacGoalStateStart !j
  | TacGoalStateUnfocused !(DerivTree h j x)
  | TacGoalStateFocused !(DerivTreeZ h j x)
  deriving (Eq, Show)

tacGoalStateGoal :: TacGoalState h j x -> (j, Evaluated)
tacGoalStateGoal tgs =
  case tgs of
    TacGoalStateStart j -> (j, EvaluatedNo)
    TacGoalStateUnfocused t -> (derivGoal t, EvaluatedYes)
    TacGoalStateFocused tz -> derivZGoal tz

data TacState h j x s = TacState
  { tsGoalState :: !(TacGoalState h j x)
  , tsThread :: !s
  } deriving (Eq, Show)

tacStateGoal :: TacState h j x s -> (j, Evaluated)
tacStateGoal = tacGoalStateGoal . tsGoalState

newtype TacticT h j x s e m a = TacticT
  { unTacticT :: BaseT (TacState h j x s) e (ListT m) a
  } deriving (
    Functor, Applicative, Monad,
    MonadError e, MonadState (TacState h j x s))

type Tactic h j x s e = TacticT h j x s e (HoleM h x)

instance Monad m => Alternative (TacticT h j x s e m) where
  empty = TacticT (lift empty)
  TacticT one <|> TacticT two = TacticT (BaseT (StateT (ExceptT . go))) where
    go s = runBaseT one s <|> runBaseT two s

instance Monad m => MonadPlus (TacticT h j x s e m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans (TacticT h j x s e) where
  lift = TacticT . lift . lift

instance MFunctor (TacticT h j x s e) where
  hoist trans = TacticT . hoist (hoist trans) . unTacticT

runTacticT :: TacticT h j x s e m a -> TacState h j x s -> m (Maybe (Either e (a, TacState h j x s), ListT m (Either e (a, TacState h j x s))))
runTacticT t s = ListT.uncons (runBaseT (unTacticT t) s)

search :: TacticT h j x s e m () -> j -> s -> ListT m (Either (DerivError h j x e) (DerivEnd h j x))
search = undefined

recursing :: Monad m => TraversalOrder -> TacticT h j x s e m () -> TacticT h j x s e m ()
recursing o t = trying (t *> trying (advanceGoal o *> recursing o t))

goal :: Monad m => TacticT h j x s e m (j, Evaluated)
goal = gets tacStateGoal

setGoalState :: Monad m => TacGoalState h j x -> TacticT h j x s e m ()
setGoalState tgs = modify' (\ts -> ts { tsGoalState = tgs })

liftNonDet :: Functor m => ListT m (Either e a) -> TacticT h j x s e m a
liftNonDet listt = TacticT (BaseT (StateT (\s -> ExceptT (fmap (fmap (,s)) listt))))

evaluateGoal :: Monad m => (j -> s -> ListT m (Either e (x, s, Seq (h, j)))) -> TacticT h j x s e m ()
evaluateGoal f = do
  TacState tgs st <- get
  case tgs of
    TacGoalStateStart j -> do
      (x, st', subs) <- liftNonDet (f j st)
      let tgs' = TacGoalStateUnfocused (startDeriv j x subs)
      put (TacState tgs' st')
    TacGoalStateUnfocused _ -> empty
    TacGoalStateFocused tz -> do
      (tz', st') <- flip stateTreeZ tz $ \_ _ e ->
        case e of
          Right _ -> empty
          Left j -> do
            (x, st', subs) <- liftNonDet (f j st)
            let tree = startDeriv j x subs
            pure (Right tree, st')
      let tgs' = TacGoalStateFocused tz'
      put (TacState tgs' st')

rule :: MonadHole h x m => (j -> RuleT h j x s e m x) -> TacticT h j x s e m ()
rule f = evaluateGoal (\j st -> ListT (fmap (fmap (,empty)) (runRuleT (f j) st)))

advanceGoal :: Monad m => TraversalOrder -> TacticT h j x s e m ()
advanceGoal order = go where
  go = do
    tgs <- gets tsGoalState
    maybe empty setGoalState (advance tgs)

  advance tgs =
    case tgs of
      TacGoalStateStart _ -> Nothing
      TacGoalStateUnfocused t -> fmap TacGoalStateFocused (firstTreeZ t)
      TacGoalStateFocused tz ->
        let mtz = case order of
              TraversalOrderDepthFirst -> depthTreeZ tz
              TraversalOrderBreadthFirst -> breadthTreeZ tz
        in fmap TacGoalStateFocused mtz

-- | Tries the given tactic
trying :: Monad m => TacticT h j x s e m () -> TacticT h j x s e m ()
trying t = t <|> pure ()

-- | Runs the given tactic zero or more times
repeating :: Monad m => TacticT h j x s e m () -> TacticT h j x s e m ()
repeating t = trying (t *> repeating t)

-- | Interleaves two tactics fairly
interleaving :: Monad m => TacticT h j x s e m a -> TacticT h j x s e m a -> TacticT h j x s e m a
interleaving (TacticT one) (TacticT two) = TacticT (BaseT (StateT (ExceptT . go))) where
  go s = interleave (runBaseT one s) (runBaseT two s)

-- | Interleaves several tactics fairly
choosing :: (Foldable f, Monad m) => f (TacticT h j x s e m a) -> TacticT h j x s e m a
choosing = foldr interleaving empty
