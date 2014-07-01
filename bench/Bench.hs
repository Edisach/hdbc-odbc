module Main where

import Criterion.Main (Benchmark, bench, defaultMain, nfIO, bgroup)

import Database.HDBC
import Database.HDBC.ODBC

import Control.Monad (forM, mapM)

main :: IO ()
main = do
  -- requires entry in odbc.ini with name "HDBC-test"
  conn <- connectODBC "DSN=HDBC-test"
  setupInsert conn
  defaultMain 
    [ benchInsertExecute conn 1000
    , benchInsertExecuteMany conn 1000
    , benchInsertExecute conn 1000 ]
  teardownInsert conn
  disconnect conn

-- Benchmarks

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
toTestInsert x = 
  [ SqlString (show x)
  , SqlInt32 (fromIntegral x)
  , SqlDouble (fromIntegral x)
  ]

benchInsertExecuteMany :: IConnection conn => conn -> Int -> Benchmark
benchInsertExecuteMany conn n = bench "InsertExecuteMany" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testInsert VALUES(?, ?, ?)"
  executeMany stmt $ map toTestInsert [1 .. n]
  commit conn

teardownInsert :: IConnection conn => conn -> IO()
teardownInsert conn = do
  run conn "DROP TABLE testInsert" []
  commit conn

