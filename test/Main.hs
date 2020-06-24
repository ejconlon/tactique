module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Judge.SimpleTest (testSimple)

main :: IO ()
main = defaultMain (testGroup "Judge" [testSimple])
