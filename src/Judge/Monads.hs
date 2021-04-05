module Judge.Monads
  ( R (..)
  , runR
  , unRunR
  , subStateR
  , P
  , runP
  , F (..)
  , runF
  , unRunF
  , G (..)
  , lowerF
  , Q (..)
  , runQ
  , unRunQ
  , lowerQ
  ) where

import Control.Applicative (empty)
import Control.Monad.Except (MonadError (..), ExceptT, runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..), StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free (FreeF (..), FreeT (..))
import Data.Bifunctor (first)
import ListT (ListT)
import qualified ListT
import Control.Monad.Trans.Maybe (MaybeT (..))

newtype R s e m a = R
  { unR :: StateT s (ExceptT e m) a
  } deriving newtype (
    Functor, Applicative, Monad,
    MonadError e, MonadState s)

instance MonadTrans (R s e) where
  lift = R . lift . lift

runR :: R s e m a -> s -> m (Either e (a, s))
runR r s = runExceptT (runStateT (unR r) s)

unRunR :: Monad m => (s -> m (Either e (a, s))) -> R s e m a
unRunR f = do
  s <- get
  eas <- lift (f s)
  case eas of
    Left e -> throwError e
    Right (a, s') -> a <$ put s'

subStateR :: Monad m => (s -> t) -> (t -> s -> s) -> R t e m a -> R s e m a
subStateR into outOf r = unRunR (\s -> fmap (fmap (fmap (`outOf` s))) (runR r (into s)))

type P r s e m a = ListT (ReaderT r (R s e m)) a

runP :: P r s e m a -> r -> R s e m (Maybe (a, P r s e m a))
runP = runReaderT . ListT.uncons

newtype F f s e m a = F
  { unF :: FreeT f (R s e m) a
  } deriving (Functor, Applicative, Monad, MonadState s, MonadError e)

instance MonadTrans (F f s e) where
  lift = F . lift . lift

runF :: (Functor f, Functor m) => F f s e m a -> s -> m (Either e (FreeF f a (F f s e m a), s))
runF f = fmap (fmap (first (fmap F))) . runR (runFreeT (unF f))

unRunF :: (Functor f, Monad m) => (s -> m (Either e (FreeF f a (F f s e m a), s))) -> F f s e m a
unRunF g = do
  s <- get
  exs <- lift (g s)
  case exs of
    Left e -> throwError e
    Right (x, s') -> put s' *> F (FreeT (pure (fmap unF x)))

newtype G f s e m a = G
  { unG :: R s e m (FreeF f a (G f s e m a))
  }

instance (Functor f, Functor m) => Functor (G f s e m) where
  fmap f = G . fmap g . unG where
    g x =
      case x of
        Pure a -> Pure (f a)
        Free fg -> Free (fmap (fmap f) fg)

lowerF :: (Functor f, Functor m) => F f s e m a -> G f s e m a
lowerF = G . fmap (fmap (lowerF . F)) . runFreeT . unF

newtype Q r s e m a = Q
  { unQ :: ReaderT r (R s e (MaybeT m)) a
  } deriving newtype (
    Functor, Applicative, Monad,
    MonadReader r, MonadError e, MonadState s)

instance MonadTrans (Q r s e) where
  lift = Q . lift . lift . lift

runQ :: Q r s e m a -> r -> s -> m (Maybe (Either e (a, s)))
runQ q r s = runMaybeT (runR (runReaderT (unQ q) r) s)

emptyQ :: Monad m => Q r s e m a
emptyQ = Q (ReaderT (const (lift empty)))

unRunQ :: Monad m => (r -> s -> m (Maybe (Either e (a, s)))) -> Q r s e m a
unRunQ f = do
  r <- ask
  s <- get
  meas <- lift (f r s)
  case meas of
    Nothing -> emptyQ
    Just eas ->
      case eas of
        Left e -> throwError e
        Right (a, s') -> a <$ put s'

lowerQ :: Monad m => Q r s e m a -> r -> R s e m (Maybe a)
lowerQ q r = unRunR (\s -> fmap (maybe (Right (Nothing, s)) (fmap (first Just))) (runQ q r s))
