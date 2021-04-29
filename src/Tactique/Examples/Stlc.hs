module Tactique.Examples.Stlc
  ( Judgment (..)
  , Type (..)
  , Term (..)
  , stlcSearch
  ) where

import Control.Monad.State.Strict (state)
import Data.List (find)
import Data.Sequence.NonEmpty (NESeq)
import Data.Void (Void)
import Tactique (DerivEnd, DerivError, HasHoles (..), HoleM, MtacT, mtacChoose, mtacRecur, mtacRule, mtacSearchFirst,
                 ruleMismatch, ruleSubgoal, runHoleM)

-- Just a very simple version of Simply Typed Lambda Calculus,
-- augmented with 'Hole' so that we can have
-- incomplete extracts.
data Term
  = Hole !Int
  | Var !String
  | Lam !String !Term
  | Pair !Term !Term
  deriving (Eq, Show)

instance HasHoles Int Term where
  fromHole = Hole
  substHoles f x =
    case x of
      Hole h -> f h
      Var _ -> pure x
      Lam n x' -> fmap (Lam n) (substHoles f x')
      Pair a b -> Pair <$> substHoles f a <*> substHoles f b

-- The type part of simply typed lambda calculus
data Type
  = TVar !String
  | TFun !Type !Type
  | TPair !Type !Type
  deriving (Eq, Show)

-- A judgement is just a context, along with a goal
data Judgment = Judgment ![(String, Type)] !Type
  deriving (Eq, Show)

type StlcM = MtacT Int Judgment Term Int Void (HoleM Int Term)
type StlcDerivError = DerivError Int Judgment Term Void
type StlcDeriv = DerivEnd Int Judgment Term

stlcPair :: StlcM ()
stlcPair = mtacRule $ \case
  Judgment hys (TPair a b) -> Pair <$> ruleSubgoal (Judgment hys a) <*> ruleSubgoal (Judgment hys b)
  _ -> ruleMismatch

stlcLam :: StlcM ()
stlcLam = mtacRule $ \case
  Judgment hys (TFun a b) -> do
    name <- state (\i -> (show i, succ i))
    body <- ruleSubgoal (Judgment ((name, a) : hys) b)
    pure (Lam name body)
  _ -> ruleMismatch

stlcAssumption :: StlcM ()
stlcAssumption = mtacRule $ \(Judgment hys a) ->
  case find (\(_, ty) -> ty == a) hys of
    Just (x, _) -> pure (Var x)
    Nothing -> ruleMismatch

stlcAuto :: StlcM ()
stlcAuto = do
    mtacChoose
      [ stlcLam *> mtacRecur stlcAuto
      , stlcPair *> mtacRecur stlcAuto
      , stlcAssumption
      ]

stlcSearch :: Judgment -> Maybe (Either (NESeq StlcDerivError) StlcDeriv)
stlcSearch j = fst (runHoleM (mtacSearchFirst stlcAuto j 0) 0)
