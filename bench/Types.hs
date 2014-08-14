module Types(benchTypes, setupTypes, teardownTypes) where

import Criterion.Main (Benchmark, bench, whnf, bgroup, nfIO)
import Database.HDBC
import Database.HDBC.SqlValue
import Data.ByteString.Char8(pack)
import qualified Data.ByteString as B
import Data.Word
import Data.Int
import Data.Ratio

-- Benchmark group
benchTypes conn = bgroup "Types" 
             [ benchToSql
             , benchIToSql
             , benchNToSql
             , benchFromSql
             --, benchSafeFromSql 
             , benchInsertStringShort conn
             , benchInsertByteStringShort conn
             , benchInsertStringLong conn
             , benchInsertByteStringLong conn
             , benchInsertStringFile conn
             , benchInsertByteStringFile conn
             , benchInsertWord32 conn
             , benchInsertWord64 conn
             , benchInsertInt64 conn
             , benchInsertInteger conn
             , benchInsertChar conn
             , benchInsertBool conn
             , benchInsertDouble conn
             , benchInsertRational conn
             ]

-- Utilities
setupTypes :: IConnection conn => conn -> IO ()
setupTypes conn = do
  run conn "DROP TABLE IF EXISTS testTypes" []
  run conn "CREATE TABLE testTypes (string TEXT)" []
  commit conn

teardownTypes :: IConnection conn => conn -> IO ()
teardownTypes conn = do
  run conn "DROP TABLE testTypes" []
  commit conn

-- Benchmarks
benchToSql = bench "toSql" $ whnf toSql "1"
benchIToSql = bench "iToSql" $ whnf iToSql 1
benchNToSql = bench "nToSql" $ whnf nToSql 1
benchFromSql = bench "fromSql" $ whnf (fromSql::SqlValue -> Int) (SqlInt64 1)

longString     = replicate   100000 'a'
longByteString = B.replicate 100000 (fromIntegral 98)
longInteger    = 10^100000

benchInsertStringShort conn = bench "insertStringShort" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlString "aaaaa"]
  commit conn

benchInsertStringLong conn = bench "insertStringLong" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlString longString]
  commit conn

benchInsertStringFile conn = bench "insertStringFile" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  str <- readFile "text"
  execute stmt [SqlString str]
  commit conn

benchInsertByteStringShort conn = bench "insertByteStringShort" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlByteString (pack "bbbbb")]
  commit conn

benchInsertByteStringLong conn = bench "insertByteStringLong" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlByteString longByteString]
  commit conn

benchInsertByteStringFile conn = bench "insertByteStringFile" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  str <- B.readFile "text"
  execute stmt [SqlByteString str]
  commit conn

benchInsertWord32 conn = bench "insertWord32" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlWord32 ((fromIntegral 97) :: Word32)]
  commit conn

benchInsertWord64 conn = bench "insertWord64" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlWord64 ((fromIntegral 98) :: Word64)]
  commit conn

benchInsertInt32 conn = bench "insertInt32" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlInt32 ((fromIntegral 1) :: Int32)]
  commit conn

benchInsertInt64 conn = bench "insertInt64" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlInt64 ((fromIntegral 2) :: Int64)]
  commit conn

benchInsertInteger conn = bench "insertInteger" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlInteger (fromIntegral 3)]
  commit conn

benchInsertIntegerLong conn = bench "insertIntegerLong" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlInteger longInteger] 

benchInsertChar conn = bench "insertChar" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlChar 'a']
  commit conn

benchInsertBool conn = bench "insertBool" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlBool True]
  commit conn

benchInsertDouble conn = bench "insertDouble" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlDouble 1.0]
  commit conn

benchInsertRational conn = bench "insertRational" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testTypes VALUES (?)"
  execute stmt [SqlRational $ 5%3]
  commit conn
