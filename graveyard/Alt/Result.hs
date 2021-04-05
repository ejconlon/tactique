module Judge.Alt.Result
  ( Result (..)
  ) where

import Data.Sequence (Seq)

data Result x s e a =
    ResultError !s !e
  | ResultSuccess !s !x !(Seq a)
  deriving (Eq, Show)
