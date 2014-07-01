module Main where

import Criterion.Main (Benchmark, bench, defaultMain, nfIO, bgroup)

import Database.HDBC
import Database.HDBC.ODBC

import Control.Monad (forM)

main :: IO ()
main = do
  -- requires entry in odbc.ini with name "HDBC-test"
  conn <- connectODBC "DSN=HDBC-test"
  setupInsert conn
  defaultMain [benchInsert conn 1000]
  teardownInsert conn
  disconnect conn

-- Benchmarks

-- Helper functions

setupInsert :: IConnection conn => conn -> IO ()
setupInsert conn = do
  run conn
    "CREATE TABLE testInsert (char CHAR(10), int INTEGER, float FLOAT)" []
  commit conn

benchInsert :: IConnection conn => conn -> Int -> Benchmark

benchInsert :: IConnection conn => conn -> Int -> IO ()
benchInsert conn n = bench "Insert" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testInsert VALUES (?, ?, ?)"
  forM [1 .. n] $ \x ->
    execute stmt
      [ SqlString (show x)
      , SqlInt32 (fromIntegral x)
      , SqlDouble (fromIntegral x)
      ]
  commit conn

teardownInsert :: IConnection conn => conn -> IO()
teardownInsert conn = do
  run conn "DROP TABLE testInsert" []
  commit conn

