module Judge.Validation
  ( ValidT (..)
  , invalid
  ) where

import Control.Applicative (liftA2)

newtype ValidT e m a = ValidT { runValidT :: m (Either e a) } deriving (Functor)

instance (Semigroup e, Applicative m) => Applicative (ValidT e m) where
  pure = ValidT . pure . Right
  liftA2 f (ValidT m1) (ValidT m2) = ValidT (liftA2 g m1 m2) where
    g (Left e1) (Left e2) = Left (e1 <> e2)
    g (Left e1) (Right _) = Left e1
    g (Right _) (Left e2) = Left e2
    g (Right a1) (Right a2) = Right (f a1 a2)

invalid :: Applicative m => e -> ValidT e m a
invalid = ValidT . pure . Left
