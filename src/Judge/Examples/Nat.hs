module Judge.Examples.Nat
  (
  ) where

import Control.Monad.Except (throwError)
import Judge (HasHole (..), Tactic, rule, subgoal)

newtype NatInt = NatInt
  { unNatInt :: Int
  } deriving (Eq, Show)

data NatUnary =
    NatUnaryZ
  | NatUnaryS !NatUnary
  | NatHole !Int
  deriving (Eq, Show)

instance HasHole Int NatUnary where
  fromHole = NatHole
  matchHole x =
    case x of
      NatHole h -> Just h
      _ -> Nothing

newtype NatError =
  NatErrorNegative Int
  deriving (Eq, Show)

type NatTac a = Tactic Int NatInt NatUnary () NatError a

natRule :: NatTac ()
natRule = rule $ \(NatInt i) ->
  if i < 0
    then throwError (NatErrorNegative i)
    else if i == 0
      then pure NatUnaryZ
      else NatUnaryS <$> subgoal (NatInt (pred i))

-- two = NatUnaryS (NatUnaryS NatUnaryZ)
-- expected = Seq.singleton (Right (two, (), Seq.empty))
-- actual = simpleSearch NatHole (repeating natRule) (NatInt 2) (
