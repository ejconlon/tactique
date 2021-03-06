-- | It's a non-empty stack. (I don't want to have to remember which end of the Seq is up!)
-- | This module is meant to be imported qualified.
module Tactique.Data.NEStack
  ( NEStack (..)
  , isBottom
  , singleton
  , push
  , peek
  , pop
  , unpop
  , modifyTop
  ) where

import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq

-- | Fold order is from top to bottom
newtype NEStack a = NEStack
  { unNEStack :: NESeq a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Semigroup (NEStack a) where
  NEStack s1 <> NEStack s2 = NEStack (s1 <> s2)

isBottom :: NEStack a -> Bool
isBottom (NEStack (_ :<|| ns)) = Seq.null ns

singleton :: a -> NEStack a
singleton = NEStack . NESeq.singleton

push :: a -> NEStack a -> NEStack a
push a = NEStack . (NESeq.<|) a . unNEStack

peek :: NEStack a -> a
peek = NESeq.head . unNEStack

pop :: NEStack a -> (a, Maybe (NEStack a))
pop (NEStack (a :<|| ns)) = (a, fmap NEStack (NESeq.nonEmptySeq ns))

unpop :: a -> Maybe (NEStack a) -> NEStack a
unpop a = maybe (singleton a) (push a)

modifyTop :: (a -> a) -> NEStack a -> NEStack a
modifyTop f (NEStack (a :<|| ns)) = NEStack (f a :<|| ns)
