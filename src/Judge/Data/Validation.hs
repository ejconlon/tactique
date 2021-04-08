module Judge.Data.Validation
  ( ValidT (..)
  , Valid
  , runValid
  , invalid
  ) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad.Identity (Identity (..))

newtype ValidT e m a = ValidT { runValidT :: m (Either e a) } deriving (Functor)

type Valid e a = ValidT e Identity a

runValid :: Valid e a -> Either e a
runValid = runIdentity . runValidT

instance (Semigroup e, Applicative m) => Applicative (ValidT e m) where
  pure = ValidT . pure . Right
  liftA2 f (ValidT m1) (ValidT m2) = ValidT (liftA2 g m1 m2) where
    g (Left e1) (Left e2) = Left (e1 <> e2)
    g (Left e1) (Right _) = Left e1
    g (Right _) (Left e2) = Left e2
    g (Right a1) (Right a2) = Right (f a1 a2)

instance (Monoid e, Applicative m) => Alternative (ValidT e m) where
  empty = ValidT (pure (Left mempty))
  ValidT m1 <|> ValidT m2 = ValidT (liftA2 g m1 m2) where
    g (Left e1) (Left e2) = Left (e1 <> e2)
    g (Left _) (Right a2) = Right a2
    g (Right a1) _ = Right a1

invalid :: Applicative m => e -> ValidT e m a
invalid = ValidT . pure . Left
