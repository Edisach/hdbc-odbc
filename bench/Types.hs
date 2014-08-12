module Types(benchTypes, setupTypes, teardownTypes) where

import Criterion.Main (Benchmark, bench, whnf, bgroup, nfIO)
import Database.HDBC
import Database.HDBC.SqlValue
import Data.ByteString.Char8(pack)
import qualified Data.ByteString as B

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
