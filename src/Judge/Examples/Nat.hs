module Judge.Examples.Nat where

import Control.Monad.Except (throwError)
import Judge (DerivEnd, DerivError, HasHole (..), HoleM, MtacT, Order (..), mtacRepeat, mtacRule, mtacSearch,
              ruleSubgoal, runHoleM)
import qualified ListT

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

type NatM = MtacT Int NatInt NatUnary () NatError (HoleM Int NatUnary)
type NatDerivError = DerivError Int NatInt NatUnary NatError
type NatDeriv = DerivEnd Int NatInt NatUnary

natRule :: NatM ()
natRule = mtacRule $ \(NatInt i) ->
  if i < 0
    then throwError (NatErrorNegative i)
    else if i == 0
      then pure NatUnaryZ
      else NatUnaryS <$> ruleSubgoal (NatInt (pred i))

natAuto :: NatM ()
natAuto = mtacRepeat DepthOrder natRule

natSearch :: NatInt -> [Either NatDerivError NatDeriv]
natSearch j = fst (runHoleM (ListT.toList (mtacSearch natAuto j ())) 0)
