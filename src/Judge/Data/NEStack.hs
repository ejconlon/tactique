-- | It's a non-empty stack. (I don't want to have to remember which end of the Seq is up!)
-- | This module is meant to be imported qualified.
module Judge.Data.NEStack
  ( NEStack (..)
  , singleton
  , push
  , peek
  , pop
  ) where

import Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq

-- | Fold order is from top to bottom
newtype NEStack a = NEStack
  { unNEStack :: NESeq a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

singleton :: a -> NEStack a
singleton = NEStack . NESeq.singleton

push :: a -> NEStack a -> NEStack a
push a = NEStack . (NESeq.<|) a . unNEStack

peek :: NEStack a -> a
peek = NESeq.head . unNEStack

pop :: NEStack a -> (a, Maybe (NEStack a))
pop (NEStack (a :<|| ns)) = (a, fmap NEStack (NESeq.nonEmptySeq ns))
