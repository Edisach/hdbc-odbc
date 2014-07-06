module Main where

import Test.Tasty
import UnitTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [hTests]

