module Judge.Search
  ( SearchF (..)
  , SearchT (..)
  , Search
  , runSearchT
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Identity (Identity)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free (MonadFree (..), FreeF (..))
import Data.Sequence (Seq (..))
import Judge.Monads (SuspT, runSuspT)
import ListT (ListT (..))
import qualified ListT

data SearchF j x a =
    SearchSubgoal !j !(x -> a)
  | SearchAlt a a
  | SearchEmpty
  deriving (Functor)

newtype SearchT j x s e m a = SearchT
  { unSearchT :: SuspT (SearchF j x) s e m a
  } deriving (
    Functor, Applicative, Monad,
    MonadState s, MonadError e)

type Search j x s e a = SearchT j x s e Identity a

instance Monad m => MonadFree (SearchF j x) (SearchT j x s e m) where
  wrap = SearchT . wrap . fmap unSearchT

instance MonadTrans (SearchT j x s e) where
  lift = SearchT . lift

instance MFunctor (SearchT j x s e) where
  hoist trans = SearchT . hoist trans . unSearchT

instance Monad m => Alternative (SearchT j x s e m) where
  empty = wrap SearchEmpty
  one <|> two = wrap (SearchAlt one two)

instance Monad m => MonadPlus (SearchT j x s e m) where
  mzero = empty
  mplus = (<|>)

runSearchT :: Monad m => m x -> SearchT j x s e m a -> s -> ListT m (Either e (a, s, Seq j))
runSearchT hole = go Empty . unSearchT where
  go !goals f s = ListT $ do
    eas <- runSuspT f s
    case eas of
      Left e -> pure (Just (Left e, empty))
      Right (fy, s') ->
        case fy of
          Pure a -> pure (Just (Right (a, s', goals), empty))
          Free y ->
            case y of
              SearchSubgoal g k -> do
                h <- hole
                ListT.uncons (go (goals :|> g) (k h) s')
              SearchAlt f1 f2 ->
                ListT.uncons (go Empty f1 s' <> go Empty f2 s')
              SearchEmpty -> pure Nothing
