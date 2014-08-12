module SCTests(sTests) where

import Test.Tasty
import Test.Tasty.SmallCheck 
import Test.Tasty.HUnit
import qualified Data.Map as Map
import Data.ByteString.Char8(pack)
import Data.Word
import Data.Int

import Database.HDBC
import Database.HDBC.ODBC
import Database.HDBC.SqlValue

import Utils

createSCTable = "CREATE TABLE testSC (typestring VARCHAR(20), typeint INTEGER, typechar CHAR(3), typebool BOOLEAN, typefloat REAL)" 

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
  , testProperty "testInsertSqlString" testInsertSqlString
  , testProperty "testInsertSqlByteString" testInsertSqlByteString
  , testProperty "testInsertSqlWord32" testInsertSqlWord32
  , testProperty "testInsertSqlWord64" testInsertSqlWord64
  , testProperty "testInsertSqlInt32" testInsertSqlInt32
  , testProperty "testInsertSqlInt64" testInsertSqlInt64
  , testProperty "testInsertSqlInteger" testInsertSqlInteger
  , testProperty "testInsertSqlChar" testInsertSqlChar
  , testProperty "testInsertSqlBool" testInsertSqlBool
  , testProperty "testInsertSqlDouble" testInsertSqlDouble
  , testProperty "testInsertSqlRational" testInsertSqlRational
  ]

testCreateTable = dbTest $ (\dbh ->
  do run dbh "DROP TABLE IF EXISTS testSC" []
     commit dbh
     run dbh createSCTable []
     commit dbh
     )

testString = (\x -> toSql (x::String) == SqlString x)

testMonadic = \x -> monadic $ dbTest $ \dbh ->
  do sth <- prepare dbh "SELECT ?"
     execute sth [toSql (x :: Int)]
     commit dbh
     v <- fetchRow sth
     return (Just [toSql x] == v)

testInsertString = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typestring) VALUES (?)"
     execute sth [toSql (x :: String)]
     commit dbh
     v <- quickQuery' dbh "SELECT typestring FROM testSC" []
     commit dbh
     return (v == [[toSql x]])

testInsertInt = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typeint) VALUES (?)"
     execute sth [toSql (x :: Int)]
     commit dbh
     v <- quickQuery' dbh "SELECT typeint FROM testSC" []
     commit dbh
     return (map (map fromSql) v == [[x]])

testInsertCharacter = \x -> length x == 3 ==> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typechar) VALUES (?)"
     execute sth [toSql (x :: [Char])]
     commit dbh
     v <- quickQuery' dbh "SELECT typechar FROM testSC" []
     commit dbh
     return (map (map fromSql) v == [[x]])

testInsertBool = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typebool) VALUES (?)"
     execute sth [toSql (x :: Bool)]
     commit dbh
     v <- quickQuery' dbh "SELECT typebool FROM testSC" []
     commit dbh
     return (map (map fromSql) v == [[x]])

testInsertDouble = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typefloat) VALUES (?)"
     execute sth [toSql (x :: Double)]
     commit dbh
     v <- quickQuery' dbh "SELECT typefloat FROM testSC" []
     commit dbh
     return (map (map fromSql) v == [[x]])

testInsertRational = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typestring) VALUES(?)"
     execute sth [toSql (x :: Rational)]
     commit dbh
     v <- quickQuery' dbh "SELECT typestring FROM testSC" []
     commit dbh
     return (map (map fromSql) v == [[x]])

testInsertSqlString = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typestring) VALUES(?)"
     execute sth [SqlString (x :: String)]
     commit dbh
     v <- quickQuery' dbh "SELECT typestring FROM testSC" []
     commit dbh
     return ([[SqlString x]] == v)

testInsertSqlByteString = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typestring) VALUES(?)"
     execute sth [SqlByteString (pack (x :: String))]
     commit dbh
     v <- quickQuery' dbh "SELECT typestring FROM testSC" []
     commit dbh
     return ([[SqlByteString (pack x)]] == v)

testInsertSqlWord32 = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typestring) VALUES(?)"
     execute sth [SqlWord32 ((fromIntegral (x :: Int)) :: Word32)]
     commit dbh
     v <- quickQuery' dbh "SELECT typestring FROM testSC" []
     commit dbh
     return ([[SqlWord32 ((fromIntegral x) :: Word32)]] == v)


testInsertSqlWord64 = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typestring) VALUES(?)"
     execute sth [SqlWord64 ((fromIntegral (x :: Int)) :: Word64)]
     commit dbh
     v <- quickQuery' dbh "SELECT typestring FROM testSC" []
     commit dbh
     return ([[SqlWord64 ((fromIntegral x) :: Word64)]] == v)


testInsertSqlInt32 = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typestring) VALUES(?)"
     execute sth [SqlInt32 ((fromIntegral (x :: Int)) :: Int32)]
     commit dbh
     v <- quickQuery' dbh "SELECT typestring FROM testSC" []
     commit dbh
     return ([[SqlInt32 ((fromIntegral x) :: Int32)]] == v)

testInsertSqlInt64 = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typestring) VALUES(?)"
     execute sth [SqlInt64 ((fromIntegral (x :: Int)) :: Int64)]
     commit dbh
     v <- quickQuery' dbh "SELECT typestring FROM testSC" []
     commit dbh
     return ([[SqlInt64 ((fromIntegral x) :: Int64)]] == v)

testInsertSqlInteger = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typeint) VALUES(?)"
     execute sth [SqlInteger (fromIntegral(x :: Int))]
     commit dbh
     v <- quickQuery' dbh "SELECT typeint FROM testSC" []
     commit dbh
     return ([[SqlInteger ((fromIntegral x))]] == v)

testInsertSqlChar = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typestring) VALUES(?)"
     execute sth [SqlChar (x :: Char)]
     commit dbh
     v <- quickQuery' dbh "SELECT typestring FROM testSC" []
     commit dbh
     return ([[SqlChar x]] == v)

testInsertSqlBool = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typebool) VALUES(?)"
     execute sth [SqlBool (x :: Bool)]
     commit dbh
     v <- quickQuery' dbh "SELECT typebool FROM testSC" []
     commit dbh
     return ([[x]] == (map (map fromSql) v))
     --return ([[SqlBool x]] == v) -- Bool storage varies by database
     --v could be e.g. a ByteString "1" for True. Use fromSql instead

testInsertSqlDouble = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typefloat) VALUES(?)"
     execute sth [SqlDouble (x :: Double)]
     commit dbh
     v <- quickQuery' dbh "SELECT typefloat FROM testSC" []
     commit dbh
     return ([[SqlDouble x]] == v)

testInsertSqlRational = \x -> monadic $ dbTest $ \dbh ->
  do run dbh "DELETE FROM testSC" []
     sth <- prepare dbh "INSERT INTO testSC(typestring) VALUES(?)"
     execute sth [SqlRational (x :: Rational)]
     commit dbh
     v <- quickQuery' dbh "SELECT typestring FROM testSC" []
     commit dbh
     return ([[SqlRational x]] == v)

