module Main where

import Test.Tasty
import UnitTests
import SCTests
import QCTests
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
        [hTests{-, sTests, qTests-}]

