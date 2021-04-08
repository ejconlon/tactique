module Judge.Examples.Stlc where

import Control.Monad.State (state)
import Data.List (find)
import Data.Void (Void)
import Judge (HasHole (..), Tactic, choosing, mismatch, repeating, rule, subgoal)

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

type T a = Tactic Int Judgment Term Int Void a

pair :: T ()
pair = rule $ \case
  Judgment hys (TPair a b) -> Pair <$> subgoal (Judgment hys a) <*> subgoal (Judgment hys b)
  _ -> mismatch

lam :: T ()
lam = rule $ \case
  Judgment hys (TFun a b) -> do
    name <- state (\i -> (show i, succ i))
    body <- subgoal (Judgment ((name, a) : hys) b)
    pure (Lam name body)
  _ -> mismatch

assumption :: T ()
assumption = rule $ \(Judgment hys a) ->
  case find (\(_, ty) -> ty == a) hys of
    Just (x, _) -> pure (Var x)
    Nothing -> mismatch

auto :: T ()
auto = do
    repeating lam
    choosing
      [ pair *> auto
      , assumption
      ]

jdg :: Judgment
jdg = Judgment [] (TFun (TVar "a") (TFun (TVar "b") (TPair (TVar "a") (TVar "b"))))

-- stlcMain :: IO ()
-- stlcMain = print (simpleSearch Hole auto jdg 0)
