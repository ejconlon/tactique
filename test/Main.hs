module Main (main) where

import Control.Applicative (empty)
import Control.Monad.Except (throwError)
import Control.Monad.Identity (Identity (..))
-- import Control.Monad.Logic.Class (interleave)
import qualified Data.Sequence as Seq
import ListT (ListT)
import qualified ListT
import Judge
import Judge.Examples.Nat
import Judge.Examples.Stlc
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

toListT :: [Int] -> ListT Identity Int
toListT = ListT.fromFoldable

fromListT :: ListT Identity Int -> [Int]
fromListT = runIdentity . ListT.toList

tests :: TestTree
tests = testGroup "judge"
  -- [ testCase "simple" $ do
  --     let expected = Seq.singleton (Right (Lam "0" (Lam "1" (Pair (Var "0") (Var "1"))), 2, Seq.empty))
  --         actual = simpleSearch Hole auto jdg 0
  --     actual @?= expected
  -- [ testCase "interleave listt" $ do
  --     let l1 = toListT [1, 2, 3]
  --         l2 = toListT [4, 5, 6]
  --         expected = [1, 4, 2, 5, 3, 6]
  --         actual = fromListT (interleave l1 l2)
  --     actual @?= expected
  [ testCase "nat" $ do
      let twoInt = NatInt 2
          twoUnary = NatUnaryS (NatUnaryS NatUnaryZ)
          -- expected = Seq.singleton (Right (two, (), Seq.empty))
          res = natSearch twoInt
      print res
      --     actual = simpleSearch NatHole (repeating natRule) (NatInt 2) ()
      -- actual @?= expected
  ]

main :: IO ()
main = defaultMain tests
