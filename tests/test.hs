module Main where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Database.HDBC
import Database.HDBC.ODBC

import Utils

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [hTests]

properties :: TestTree
properties = testGroup "Properties" []

scProps = testGroup "Checked by SmallCheck" []

createTable = dbTest $ (\dbh ->
      do run dbh "CREATE TABLE test1 (name VARCHAR(10), id INTEGER)" []
         commit dbh
         )

dropTable = dbTest $ (\dbh ->
      do run dbh "DROP TABLE test1" []
         commit dbh
         )

definiteFail = assertEqual "much fail" 1 2 

hTests = testGroup "HUnit Tests" 
         [ testCase "Create table" createTable
         , testCase "Drop table" dropTable
         , testCase "Fail" definiteFail
         ]
