module Test.Judge.SimpleTest (testSimple) where

import Test.Tasty
import Test.Tasty.HUnit

testSimple :: TestTree
testSimple = testCase "simple" $ do
    let actual = (1 + 1) :: Int
        expected = 2 :: Int
    actual @?= expected
