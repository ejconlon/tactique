module Judge.Handler.Internal
  ( NatTrans (..)
  , idTrans
  , pureTrans
  , StatePair (..)
  ) where

import Control.Monad.Identity (Identity (..))

newtype NatTrans m n = NatTrans { runNatTrans :: forall a. m a -> n a }

idTrans :: NatTrans m m
idTrans = NatTrans id

pureTrans :: Monad n => NatTrans Identity n
pureTrans = NatTrans (pure . runIdentity)

data StatePair s a =
  StatePair !s a
  deriving (Eq, Show, Functor)
