module Tactique.Mtac
  ( MtacT (..)
  , Order (..)
  , mtacSearch
  , mtacSearchFirst
  , mtacGoal
  , mtacRule
  , mtacEvaluate
  , mtacTry
  , mtacRecur
  , mtacRestrict
  , mtacRepeat
  , mtacOnce
  , mtacNextGoal
  , mtacNextUnevaluatedGoal
  , mtacChoose
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
-- import Control.Monad.Logic.Class (MonadLogic (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets, modify')
import Control.Monad.Trans (MonadTrans (..))
import Data.Either (partitionEithers)
import Data.Foldable (asum)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Sequence.NonEmpty as NESeq
import Data.Traversable (for)
import ListT (ListT (..))
import qualified ListT
import Tactique.Data.TreeZ (Tree (..), TreeF (..), breadthTreeZ, depthTreeZ, firstTreeZ, outTreeZ, pruneTree, readTreeZ,
                            stateTreeZ, writeTreeZ)
import Tactique.Derivation (DerivEnd, DerivError (..), DerivTree, DerivTreeZ, Evaluated (..), derivGoal, derivZGoal,
                            startDeriv)
import Tactique.Holes (MonadHole)
import Tactique.Monads (BaseT (..), runBaseT)
import Tactique.Rule (RuleT)
import Tactique.Tac (TacResT, TacT (..), runTacT, tacRule)

data Order = DepthOrder | BreadthOrder deriving (Eq, Show)

data MtacGoalState h j x =
    MtacGoalStateStart !j
  | MtacGoalStateUnfocused !(DerivTree h j x)
  | MtacGoalStateFocused !(DerivTreeZ h j x)
  deriving (Eq, Show)

mtacGoalStateGoal :: MtacGoalState h j x -> (j, Evaluated)
mtacGoalStateGoal mgs =
  case mgs of
    MtacGoalStateStart j -> (j, EvaluatedNo)
    MtacGoalStateUnfocused t -> (derivGoal t, EvaluatedYes)
    MtacGoalStateFocused tz -> derivZGoal tz

data MtacState h j x s = MtacState
  { msGoalState :: !(MtacGoalState h j x)
  , msThread :: !s
  } deriving (Eq, Show)

mtacStateGoal :: MtacState h j x s -> (j, Evaluated)
mtacStateGoal = mtacGoalStateGoal . msGoalState

newtype MtacT h j x s e m a = MtacT
  { unMtacT :: BaseT (MtacState h j x s) e (ListT m) a
  } deriving (
    Functor, Applicative, Monad,
    MonadError e, MonadState (MtacState h j x s))

instance Monad m => Alternative (MtacT h j x s e m) where
  empty = MtacT (lift empty)
  MtacT one <|> MtacT two = MtacT (BaseT (StateT (ExceptT . go))) where
    go s = runBaseT one s <|> runBaseT two s

instance Monad m => MonadPlus (MtacT h j x s e m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans (MtacT h j x s e) where
  lift = MtacT . lift . lift

instance MFunctor (MtacT h j x s e) where
  hoist trans = MtacT . hoist (hoist trans) . unMtacT

instance MonadIO m => MonadIO (MtacT h j x s e m) where
  liftIO = lift . liftIO

-- instance Monad m => MonadLogic (MtacT h j x s e m) where
--   msplit = undefined

runMtacT :: MtacT h j x s e m a -> MtacState h j x s -> ListT m (Either e (a, MtacState h j x s))
runMtacT = runBaseT . unMtacT

mtacSearch :: Functor m => MtacT h j x s e m () -> j -> s -> ListT m (Either (DerivError h j x e) (DerivEnd h j x))
mtacSearch m j s = fmap go (runMtacT m (MtacState (MtacGoalStateStart j) s)) where
  go res =
    case res of
      Left e -> Left (DerivCustomError e)
      Right (_, MtacState mgs _) -> do
        t <- case mgs of
          MtacGoalStateStart j' -> Left (DerivUnevalError j')
          MtacGoalStateUnfocused t -> Right t
          MtacGoalStateFocused tz -> Right (outTreeZ tz)
        either (Left . DerivSolveError) Right (pruneTree t)

mtacSearchFirst :: Monad m => MtacT h j x s e m () -> j -> s -> m (Maybe (Either (NESeq (DerivError h j x e)) (DerivEnd h j x)))
mtacSearchFirst m j s = fmap go (ListT.toList (mtacSearch m j s)) where
  go eas =
    let (ls, rs) = partitionEithers eas
    in if null rs
      then if null ls
        then Nothing
        else Just (Left (NESeq.unsafeFromSeq (Seq.fromList ls)))
      else Just (Right (head rs))

askGoalState :: Monad m => MtacT h j x s e m (Either j (DerivTree h j x))
askGoalState = do
  mgs <- gets msGoalState
  let ejt = case mgs of
        MtacGoalStateStart j -> Left j
        MtacGoalStateUnfocused t -> Right t
        MtacGoalStateFocused tz' -> Right (outTreeZ tz')
  pure ejt

-- | Runs the given mtac for each subgoal and ends focus in the same place.
mtacRecur :: Monad m => MtacT h j x s e m () -> MtacT h j x s e m ()
mtacRecur m = go where
  go = do
    mgs <- gets msGoalState
    case mgs of
      MtacGoalStateStart _ -> empty
      MtacGoalStateUnfocused t -> do
        t' <- goTree t
        setGoalState (MtacGoalStateUnfocused t')
      MtacGoalStateFocused tz -> do
        let (_, _, ejt) = readTreeZ tz
        case ejt of
          Left _ -> empty
          Right t -> do
            t' <- goTree t
            let tz' = writeTreeZ (Right t') tz
            setGoalState (MtacGoalStateFocused tz')

  goTree (Tree (TreeF l hjts)) = do
    hjts' <- for hjts $ \(h, ejt) -> do
      setGoalState (either MtacGoalStateStart MtacGoalStateUnfocused ejt)
      m
      fmap (h,) askGoalState
    pure (Tree (TreeF l hjts'))

-- | Runs the given mtac in in the context of the derivation tree
-- rerooted at the current focus.
mtacRestrict :: Monad m => MtacT h j x s e m () -> MtacT h j x s e m ()
mtacRestrict m = do
  mgsStart <- gets msGoalState
  mgsEnd <- case mgsStart of
    MtacGoalStateFocused tz -> do
      let (_, _, ejt) = readTreeZ tz
          mgsMid = either MtacGoalStateStart MtacGoalStateUnfocused ejt
      setGoalState mgsMid
      m
      ejt' <- askGoalState
      let tz' = writeTreeZ ejt' tz
      pure (MtacGoalStateFocused tz')
    _ -> do
      m
      fmap (either MtacGoalStateStart MtacGoalStateUnfocused) askGoalState
  setGoalState mgsEnd

mtacRepeat :: Monad m => Order -> MtacT h j x s e m () -> MtacT h j x s e m ()
mtacRepeat o m = mtacTry (m *> mtacTry (mtacNextUnevaluatedGoal o *> mtacRepeat o m))

mtacOnce :: Monad m => Order -> MtacT h j x s e m () -> MtacT h j x s e m ()
mtacOnce o m = m *> mtacTry (mtacNextUnevaluatedGoal o)

mtacGoal :: Monad m => MtacT h j x s e m (j, Evaluated)
mtacGoal = gets mtacStateGoal

setGoalState :: Monad m => MtacGoalState h j x -> MtacT h j x s e m ()
setGoalState mgs = modify' (\ms -> ms { msGoalState = mgs })

liftTacResT :: Functor m => TacResT h j s e m a -> MtacT h j x s e m (a, s, Seq (h, j))
liftTacResT listt = MtacT (BaseT (StateT (\s -> ExceptT (fmap (fmap (,s)) listt))))

mtacEvaluate :: Monad m => TacT h j s e m x -> MtacT h j x s e m ()
mtacEvaluate t = do
  MtacState mgs st <- get
  case mgs of
    MtacGoalStateStart j -> do
      (x, st', subs) <- liftTacResT (runTacT t j st)
      let mgs' = MtacGoalStateUnfocused (startDeriv j x subs)
      put (MtacState mgs' st')
    MtacGoalStateUnfocused _ -> empty
    MtacGoalStateFocused tz -> do
      (tz', st') <- flip stateTreeZ tz $ \_ _ e ->
        case e of
          Right _ -> empty
          Left j -> do
            (x, st', subs) <- liftTacResT (runTacT t j st)
            let tree = startDeriv j x subs
            pure (Right tree, st')
      let tgs' = MtacGoalStateFocused tz'
      put (MtacState tgs' st')

mtacRule :: MonadHole h x m => (j -> RuleT j x s e m x) -> MtacT h j x s e m ()
mtacRule = mtacEvaluate . tacRule

-- | Rewind to the root goal.
mtacRewind :: Monad m => MtacT h j x s e m ()
mtacRewind = do
  mgs <- gets msGoalState
  case mgs of
    MtacGoalStateFocused tz -> do
      let mgs' = MtacGoalStateUnfocused (outTreeZ tz)
      setGoalState mgs'
    _ -> pure ()

-- | Advance to the next goal in the given traversal order.
-- Empty if goal does not exist
mtacNextGoal :: Monad m => Order -> MtacT h j x s e m ()
mtacNextGoal order = go where
  go = do
    tgs <- gets msGoalState
    maybe empty setGoalState (advance tgs)

  advance tgs =
    case tgs of
      MtacGoalStateStart _ -> Nothing
      MtacGoalStateUnfocused t -> fmap MtacGoalStateFocused (firstTreeZ t)
      MtacGoalStateFocused tz ->
        let mtz = case order of
              DepthOrder -> depthTreeZ tz
              BreadthOrder -> breadthTreeZ tz
        in fmap MtacGoalStateFocused mtz

-- | Advance to the next unevaluated goal in the given traversal order.
-- Empty if goal does not exist.
mtacNextUnevaluatedGoal :: Monad m => Order -> MtacT h j x s e m ()
mtacNextUnevaluatedGoal o = go where
  go = do
    mtacNextGoal o
    (_, ev) <- mtacGoal
    case ev of
      EvaluatedNo -> pure ()
      EvaluatedYes -> go

-- | Tries the given mtac
mtacTry :: Monad m => MtacT h j x s e m () -> MtacT h j x s e m ()
mtacTry t = t <|> pure ()

-- | Interleaves several tactics
mtacChoose :: (Foldable f, Monad m) => f (MtacT h j x s e m a) -> MtacT h j x s e m a
mtacChoose = asum -- foldr interleave empty
