module Main (main) where

import Judge (derivSubst)
import Judge.Examples.Nat (NatInt (..), NatUnary (..), natSearch)
-- import Judge.Examples.Stlc (Judgment (..), Term (..), Type (..), stlcSearch)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "judge"
  [ testCase "nat" $ do
      let twoInt = NatInt 2
          twoUnary = NatUnaryS (NatUnaryS NatUnaryZ)
      case natSearch twoInt of
        Nothing -> fail "No result"
        Just (Left errs) -> fail ("Bad: " <> show errs)
        Just (Right deriv) -> do
          let expected = Right twoUnary
              actual = derivSubst deriv
          actual @?= expected
  -- , testCase "stlc" $ do
  --     let tyJdg = Judgment [] (TFun (TVar "a") (TFun (TVar "b") (TPair (TVar "a") (TVar "b"))))
  --         tm = Lam "0" (Lam "1" (Pair (Var "0") (Var "1")))
  --     case stlcSearch tyJdg of
  --       Nothing -> fail "No result"
  --       Just (Left errs) -> fail ("Bad: " <> show errs)
  --       Just (Right deriv) -> do
  --         let expected = Right tm
  --             actual = derivSubst deriv
  --         actual @?= expected
  ]

main :: IO ()
main = defaultMain tests
