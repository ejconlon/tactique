{-# LANGUAGE ScopedTypeVariables #-}

module Judge.Tac
  ( TacSubgoals
  , TacT (..)
  , TacResT
  , runTacT
  , unRunTacT
  , tacGoal
  , tacRule
  , tacSubgoal
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logic.Class (MonadLogic (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer.Strict (MonadWriter (..), WriterT (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Judge.Holes (MonadHole)
import Judge.Monads (BaseT (..), runBaseT)
import Judge.Rule (RuleT (..), runRuleT)
import ListT (ListT (..))
import qualified ListT

type TacSubgoals h j = Seq (h, j)

newtype TacT h j s e m a = TacT
  { unTacT :: ReaderT j (WriterT (Seq (h, j)) (BaseT s e (ListT m))) a
  } deriving (
    Functor, Applicative, Monad, MonadReader j,
    MonadError e, MonadState s, MonadWriter (TacSubgoals h j))

type TacResT h j s e m a = ListT m (Either e (a, s, TacSubgoals h j))

instance Monad m => Alternative (TacT h j s e m) where
  empty = TacT (lift (lift (lift empty)))
  one <|> two = TacT (ReaderT (\j -> WriterT (BaseT (StateT (ExceptT . go j))))) where
    go j s = unwrapTacT one j s <|> unwrapTacT two j s

instance Monad m => MonadPlus (TacT h j s e m) where
  mzero = empty
  mplus = (<|>)

instance Monad m => MonadLogic (TacT h j s e m) where
  msplit t = wrapTacT (\j s -> go (unwrapTacT t j s)) where
    go = ListT . fmap (fmap go2) . ListT.uncons
    go2 (z, u) =
      case z of
        Left e -> (Left e, go u)
        Right ((a, gs), s) -> (Right ((Just (a, go3 u), gs), s), empty)
    go3 r = wrapTacT (\_ _ -> r)

instance MonadTrans (TacT h j s e) where
  lift = TacT . lift . lift . lift . lift

instance MFunctor (TacT h j s e) where
  hoist trans = TacT . hoist (hoist (hoist (hoist trans))) . unTacT

instance MonadIO m => MonadIO (TacT h j s e m) where
  liftIO = lift . liftIO

unwrapTacT :: TacT h j s e m a -> j -> s -> ListT m (Either e ((a, TacSubgoals h j), s))
unwrapTacT t j = runBaseT (runWriterT (runReaderT (unTacT t) j))

wrapTacT :: (j -> s -> ListT m (Either e ((a, TacSubgoals h j), s))) -> TacT h j s e m a
wrapTacT f = TacT (ReaderT (\j -> WriterT (BaseT (StateT (ExceptT . f j)))))

runTacT :: Functor m => TacT h j s e m a -> j -> s -> TacResT h j s e m a
runTacT t j s = fmap (fmap (\((a, w), u) -> (a, u, w))) (unwrapTacT t j s)

unRunTacT :: Functor m => (j -> s -> TacResT h j s e m a) -> TacT h j s e m a
unRunTacT f = wrapTacT (\j s -> fmap (fmap (\(a, u, w) -> ((a, w), u))) (f j s))

tacGoal :: Monad m => TacT h j s e m j
tacGoal = ask

tacRule :: MonadHole h x m => (j -> RuleT j x s e m a) -> TacT h j s e m a
tacRule f = unRunTacT (\j st -> ListT (fmap (fmap (,empty)) (runRuleT (f j) st)))

tacSubgoal :: Monad m => h -> j -> TacT h j s e m ()
tacSubgoal h j = tell (Seq.singleton (h, j))
