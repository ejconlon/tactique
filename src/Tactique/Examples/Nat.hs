module Tactique.Examples.Nat
  ( NatInt (..)
  , NatUnary (..)
  , natSearch
  ) where

import Control.Monad.Except (throwError)
import Data.Sequence.NonEmpty (NESeq)
import Tactique (DerivEnd, DerivError, HasHoles (..), HoleM, MtacT, Order (..), mtacRepeat, mtacRule, mtacSearchFirst,
                 ruleSubgoal, runHoleM)

newtype NatInt = NatInt
  { unNatInt :: Int
  } deriving (Eq, Show)

data NatUnary =
    NatUnaryZ
  | NatUnaryS !NatUnary
  | NatHole !Int
  deriving (Eq, Show)

instance HasHoles Int NatUnary where
  fromHole = NatHole
  substHoles f u =
    case u of
      NatHole h -> f h
      NatUnaryS u' -> fmap NatUnaryS (substHoles f u')
      NatUnaryZ -> pure u

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

natSearch :: NatInt -> Maybe (Either (NESeq NatDerivError) NatDeriv)
natSearch j = fst (runHoleM (mtacSearchFirst natAuto j ()) 0)
