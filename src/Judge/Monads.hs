module Judge.Monads
  ( BaseT (..)
  , runBaseT
  , SuspT (..)
  , runSuspT
  , transSuspT
  ) where

import Control.Monad.Except (MonadError (..), ExceptT, runExceptT)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State (MonadState (..), StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free (FreeF (..), FreeT (..), MonadFree (..), transFreeT)
import Data.Bifunctor (first)
import Judge.Orphans ()

newtype BaseT s e m a = BaseT
  { unBaseT :: StateT s (ExceptT e m) a
  } deriving newtype (
    Functor, Applicative, Monad,
    MonadError e, MonadState s)

instance MonadTrans (BaseT s e) where
  lift = BaseT . lift . lift

instance MFunctor (BaseT s e) where
  hoist trans = BaseT . hoist (hoist trans) . unBaseT

runBaseT :: BaseT s e m a -> s -> m (Either e (a, s))
runBaseT r s = runExceptT (runStateT (unBaseT r) s)

-- unRunR :: Monad m => (s -> m (Either e (a, s))) -> R s e m a
-- unRunR f = do
--   s <- get
--   eas <- lift (f s)
--   case eas of
--     Left e -> throwError e
--     Right (a, s') -> a <$ put s'

-- subStateR :: Monad m => (s -> t) -> (t -> s -> s) -> R t e m a -> R s e m a
-- subStateR into outOf r = unRunR (\s -> fmap (fmap (fmap (`outOf` s))) (runR r (into s)))

newtype SuspT f s e m a = SuspT
  { unSuspT :: FreeT f (BaseT s e m) a
  } deriving (Functor, Applicative, Monad, MonadState s, MonadError e)

instance (Functor f, Monad m) => MonadFree f (SuspT f s e m) where
  wrap = SuspT . wrap . fmap unSuspT

instance MonadTrans (SuspT f s e) where
  lift = SuspT . lift . lift

instance Functor f => MFunctor (SuspT f s e) where
  hoist trans = SuspT . hoist (hoist trans) . unSuspT

runSuspT :: (Functor f, Functor m) => SuspT f s e m a -> s -> m (Either e (FreeF f a (SuspT f s e m a), s))
runSuspT f = fmap (fmap (first (fmap SuspT))) . runBaseT (runFreeT (unSuspT f))

transSuspT :: (Monad m, Functor g) => (forall z. f z -> g z) -> SuspT f s e m a -> SuspT g s e m a
transSuspT trans = SuspT . transFreeT trans . unSuspT

-- unRunF :: (Functor f, Monad m) => (s -> m (Either e (FreeF f a (F f s e m a), s))) -> F f s e m a
-- unRunF g = do
--   s <- get
--   exs <- lift (g s)
--   case exs of
--     Left e -> throwError e
--     Right (x, s') -> put s' *> F (FreeT (pure (fmap unF x)))
