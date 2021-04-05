module Judge.Internal
  ( NEStack (..)
  ) where

import Data.Sequence.NonEmpty (NESeq)

newtype NEStack a = NEStack
  { unNEStack :: NESeq a
  } deriving (Eq, Show, Functor, Foldable, Traversable)
