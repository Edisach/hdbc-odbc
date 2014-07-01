module Main where

import Criterion.Main (Benchmark, bench, defaultMain, nfIO, bgroup)

import Database.HDBC
import Database.HDBC.ODBC

import Control.Monad (forM, mapM, mapM_)

main :: IO ()
main = do
  -- requires entry in odbc.ini with name "HDBC-test"
  conn <- connectODBC "DSN=HDBC-test"
  setupInsert conn
  setupSelect conn 10000
  defaultMain 
    [ benchInsert conn 1000
    , benchSelect conn 5000]
  teardownInsert conn
  teardownSelect conn
  disconnect conn

-- Benchmark groups
benchInsert conn n = bgroup "Insert" 
              [ benchInsertExecuteMany conn n
              , benchInsertExecute conn n 
              , benchInsertExecuteMany conn n ]

benchSelect conn n = bgroup "Select"
              [ benchSelectQuickQuery' conn n
              , benchSelectExecute conn n ]

-- Helper functions

setupInsert :: IConnection conn => conn -> IO ()
setupInsert conn = do
  run conn
    "CREATE TABLE testInsert (char CHAR(10), int INTEGER, float FLOAT)" []
  commit conn

benchInsertExecute :: IConnection conn => conn -> Int -> Benchmark
benchInsertExecute conn n = bench "InsertExecute" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testInsert VALUES (?, ?, ?)"
  mapM_ (execute stmt) $ map toTestInsert [1 .. n]
  commit conn

toTestInsert :: Int -> [SqlValue]
toTestInsert x = [ SqlString (show x)
  , SqlInt32 (fromIntegral x)
  , SqlDouble (fromIntegral x)]

benchInsertExecuteMany :: IConnection conn => conn -> Int -> Benchmark
benchInsertExecuteMany conn n = bench "InsertExecuteMany" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testInsert VALUES(?, ?, ?)"
  executeMany stmt $ map toTestInsert [1 .. n]
  commit conn

teardownInsert :: IConnection conn => conn -> IO()
teardownInsert conn = do
  run conn "DROP TABLE testInsert" []
  commit conn


setupSelect :: IConnection conn => conn -> Int -> IO ()
setupSelect conn n = do
  run conn
    "CREATE TABLE testSelect (char CHAR(10), int INTEGER, float FLOAT)" []
  commit conn
  stmt <- prepare conn "INSERT INTO testSelect VALUES (?, ?, ?)"
  executeMany stmt $ map toTestInsert (replicate n 1)
  commit conn

benchSelectQuickQuery' :: IConnection conn => conn -> Int -> Benchmark
benchSelectQuickQuery' conn n = bench "SelectQuickQuery'" $ nfIO $ do
  vs <- quickQuery' conn 
    "SELECT int FROM testSelect LIMIT ?" [SqlInt32 (fromIntegral n)]
  commit conn
  if ((sum . map (\[v] -> fromSql v :: Int)) vs == n)
    then return ()
    else error "benchSelectQuickQuery': Unexpected sum!"

benchSelectExecute :: IConnection conn => conn -> Int -> Benchmark
benchSelectExecute conn n = bench "SelectExecute" $ nfIO $ do
  stmt <- prepare conn 
    "SELECT int FROM testSelect LIMIT ?"
  execute stmt [SqlInt32 (fromIntegral n)]
  vs <- fetchAllRows stmt
  commit conn
  if ((sum . map (\[v] -> fromSql v :: Int)) vs == n)
    then return ()
    else error "benchSelectExecute: Unexpected sum!"

teardownSelect :: IConnection conn => conn -> IO()
teardownSelect conn = do
  run conn "DROP TABLE selectInsert" []
  commit conn


