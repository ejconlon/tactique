module Judge.Internal
  ( NatTrans (..)
  , idTrans
  , pureTrans
  , StatePair (..)
  , NEStack (..)
  ) where

import Control.Monad.Identity (Identity (..))
import Data.Sequence.NonEmpty (NESeq)

newtype NatTrans m n = NatTrans { runNatTrans :: forall a. m a -> n a }

idTrans :: NatTrans m m
idTrans = NatTrans id

pureTrans :: Monad n => NatTrans Identity n
pureTrans = NatTrans (pure . runIdentity)

data StatePair s a =
  StatePair !s a
  deriving (Eq, Show, Functor)

newtype NEStack a = NEStack
  { unNEStack :: NESeq a
  } deriving (Eq, Show, Functor, Foldable, Traversable)
