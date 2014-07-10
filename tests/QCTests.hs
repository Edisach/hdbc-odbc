module QCTests(qTests) where

import Test.Tasty
import Test.Tasty.QuickCheck 
import qualified Data.Map as Map

import Database.HDBC
import Database.HDBC.ODBC

import Utils

qTests = testGroup "QuickCheck tests"
  [ testProperty "testString" testString
  , testProperty "testProp" testProp
  , testProperty "testIO" testIO
  ]

testString = (\x -> toSql (x::String) == SqlString x)

testProp = property (\x -> (x::String) == x)

testIO = ioProperty $ dbTest $ (\dbh -> 
         do return (\x -> (x::String) == x)
            )

