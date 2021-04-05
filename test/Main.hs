module Main (main) where

import Control.Applicative (empty)
import Control.Monad.Except (throwError)
import Judge
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
-- import Judge.Examples.Stlc
import Control.Monad.Identity (Identity (..))
import ListT (ListT)
import qualified ListT
import Control.Monad.Logic.Class (interleave)
import qualified Data.Sequence as Seq

toListT :: [Int] -> ListT Identity Int
toListT = ListT.fromFoldable

fromListT :: ListT Identity Int -> [Int]
fromListT = runIdentity . ListT.toList

newtype NatInt = NatInt Int deriving (Eq, Show)
data NatUnary = NatUnaryZ | NatUnaryS !NatUnary | NatHole deriving (Eq, Show)
newtype NatError = NatErrorNegative Int deriving (Eq, Show)

type NatTac a = Tactic NatUnary NatUnary NatError () a
type NatRule a = Refine NatUnary NatUnary NatError () a

tests :: TestTree
tests = testGroup "judge"
  -- [ testCase "simple" $ do
  --     let expected = Seq.singleton (Right (Lam "0" (Lam "1" (Pair (Var "0") (Var "1"))), 2, Seq.empty))
  --         actual = simpleSearch Hole auto jdg 0
  --     actual @?= expected
  [ testCase "interleave listt" $ do
      let l1 = toListT [1, 2, 3]
          l2 = toListT [4, 5, 6]
          expected = [1, 4, 2, 5, 3, 6]
          actual = fromListT (interleave l1 l2)
      actual @?= expected
  , testCase "refine" $ do
      let natRule = rule $ \(NatInt i) ->
            if i < 0
              then throwError (NatErrorNegative i)
              else if i == 0
                then pure NatUnaryZ
                else NatUnaryS <$> subgoal (NatInt (pred i))
          two = NatUnaryS (NatUnaryS NatUnaryZ)
          expected = Seq.singleton (Right (two, (), Seq.empty))
          actual = simpleSearch NatHole (repeating natRule) (NatInt 2) ()
      actual @?= expected
  ]

main :: IO ()
main = defaultMain tests
