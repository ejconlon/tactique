module Judge.Alt.Example
  (
  ) where

import Control.Monad.Except (throwError)
import Judge.Alt.Rule (Rule, subgoal)
import Judge.Alt.Tactic (Tactic, rule)

newtype NatInt = NatInt
  { unNatInt :: Int
  } deriving (Eq, Show)

data NatUnary =
    NatUnaryZ
  | NatUnaryS !NatUnary
  | NatHole
  deriving (Eq, Show)

newtype NatError =
  NatErrorNegative Int
  deriving (Eq, Show)

-- type NatRefine a = Refine NatUnary NatUnary NatError a
type NatTac a = Tactic NatInt NatUnary () NatError a
type NatRule a = Rule NatInt NatUnary () NatError a

natRule :: NatTac ()
natRule =
  rule $ \(NatInt i) ->
    if i < 0
      then throwError (NatErrorNegative i)
      else if i == 0
        then pure NatUnaryZ
        else NatUnaryS <$> subgoal (NatInt (pred i))

-- two = NatUnaryS (NatUnaryS NatUnaryZ)
-- expected = Seq.singleton (Right (two, (), Seq.empty))
-- actual = simpleSearch NatHole (repeating natRule) (NatInt 2) (
