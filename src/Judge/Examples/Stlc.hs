module Judge.Examples.Stlc where

import Control.Monad.State.Strict (state)
import Data.List (find)
import Data.Void (Void)
import Judge (DerivEnd, DerivError, HasHole (..), HoleM, MtacT, Order (..), mtacChoose, mtacOnce, mtacRepeat, mtacRule,
              mtacSearch, ruleMismatch, ruleSubgoal, runHoleM)
import qualified ListT

-- Just a very simple version of Simply Typed Lambda Calculus,
-- augmented with 'Hole' so that we can have
-- incomplete extracts.
data Term
  = Hole !Int
  | Var !String
  | Lam !String !Term
  | Pair !Term !Term
  deriving (Eq, Show)

instance HasHole Int Term where
  fromHole = Hole
  matchHole x =
    case x of
      Hole h -> Just h
      _ -> Nothing

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
    mtacRepeat DepthOrder stlcLam
    mtacChoose
      [ mtacOnce DepthOrder stlcPair *> stlcAuto
      , mtacOnce DepthOrder stlcAssumption
      ]

jdg :: Judgment
jdg = Judgment [] (TFun (TVar "a") (TFun (TVar "b") (TPair (TVar "a") (TVar "b"))))

stlcSearch :: Judgment -> [Either StlcDerivError StlcDeriv]
stlcSearch j = fst (runHoleM (ListT.toList (mtacSearch stlcAuto j 0)) 0)
