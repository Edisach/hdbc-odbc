module SCTests(sTests) where

import Test.Tasty
import Test.Tasty.SmallCheck 
import Test.Tasty.HUnit
import qualified Data.Map as Map

import Database.HDBC
import Database.HDBC.ODBC

import Utils

createSCTable = "CREATE TABLE testSC (string VARCHAR(10), int INTEGER, char CHAR(3), bool BOOLEAN, double DOUBLE)" 

sTests = testGroup "SmallCheck tests"
  [ testCase "Create table" testCreateTable
  , testProperty "testString" testString
  , testProperty "testMonadic" testMonadic
  , testProperty "testInsertString" testInsertString
  , testProperty "testInsertInt" testInsertInt
  , testProperty "testInsertCharacter" testInsertCharacter
  , testProperty "testInsertBool" testInsertBool
  , testProperty "testInsertDouble" testInsertDouble
  , testProperty "testInsertRational" testInsertRational
  ]

testCreateTable = dbTest $ (\dbh ->
  do run dbh "DROP TABLE IF EXISTS testSC" []
     commit dbh
     run dbh createSCTable []
     commit dbh
     )

testString = (\x -> toSql (x::String) == SqlString x)

testMonadic = (\x -> monadic $ dbTest $ (\dbh ->
  do sth <- prepare dbh "SELECT ?"
     execute sth [toSql (x :: Int)]
     commit dbh
     v <- fetchRow sth
     return (Just [toSql x] == v)
     ))

testInsertString = (\x -> monadic $ dbTest $ (\dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(string) VALUES (?)"
     execute sth [toSql (x :: String)]
     commit dbh
     v <- quickQuery' dbh "SELECT string FROM testSC" []
     commit dbh
     return (map (map fromSql) v == [[x]])
     ))

testInsertInt = (\x -> monadic $ dbTest $ (\dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(int) VALUES (?)"
     execute sth [toSql (x :: Int)]
     commit dbh
     v <- quickQuery' dbh "SELECT int FROM testSC" []
     commit dbh
     return (map (map fromSql) v == [[x]])
     ))

testInsertCharacter = (\x -> length x == 3 ==> monadic $ dbTest $ (\dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(char) VALUES (?)"
     execute sth [toSql (x :: [Char])]
     commit dbh
     v <- quickQuery' dbh "SELECT char FROM testSC" []
     commit dbh
     return (map (map fromSql) v == [[x]])
     ))

testInsertBool = (\x -> monadic $ dbTest $ (\dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(bool) VALUES (?)"
     execute sth [toSql (x :: Bool)]
     commit dbh
     v <- quickQuery' dbh "SELECT bool FROM testSC" []
     commit dbh
     return (map (map fromSql) v == [[x]])
     ))

testInsertDouble = (\x -> monadic $ dbTest $ (\dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(double) VALUES (?)"
     execute sth [toSql (x :: Double)]
     commit dbh
     v <- quickQuery' dbh "SELECT double FROM testSC" []
     commit dbh
     return (map (map fromSql) v == [[x]])
     ))

testInsertRational = (\x -> monadic $ dbTest $ (\dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(string) VALUES(?)"
     execute sth [toSql (x :: Rational)]
     commit dbh
     v <- quickQuery' dbh "SELECT string FROM testSC" []
     commit dbh
     return (map (map fromSql) v == [[x]])
     ))
