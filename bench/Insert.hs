module Insert(setupInsert, teardownInsert, benchInsert) where

import Criterion.Main (Benchmark, bench, nfIO, bgroup)
import Database.HDBC
import Control.Monad (forM, mapM, mapM_)

-- The benchmark group
benchInsert conn n = bgroup "Insert" 
              [ benchInsertExecuteMany conn n
              , benchInsertExecute conn n 
              , benchInsertRun conn n
              , benchInsertExecuteMany conn n ]

-- Utilities
setupInsert :: IConnection conn => conn -> IO ()
setupInsert conn = do
  run conn "DROP TABLE IF EXISTS testInsert" []
  run conn
    "CREATE TABLE testInsert (chars CHAR(10), ints INTEGER, floats FLOAT)" []
  commit conn

teardownInsert :: IConnection conn => conn -> IO()
teardownInsert conn = do
  run conn "DROP TABLE testInsert" []
  commit conn

toTestInsert :: Int -> [SqlValue]
toTestInsert x = [ SqlString (show x)
  , SqlInt32 (fromIntegral x)
  , SqlDouble (fromIntegral x)]

-- The benchmarks
benchInsertExecute :: IConnection conn => conn -> Int -> Benchmark
benchInsertExecute conn n = bench "InsertExecute" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testInsert VALUES (?, ?, ?)"
  mapM_ (execute stmt) $ map toTestInsert [1 .. n]
  commit conn

benchInsertExecuteMany :: IConnection conn => conn -> Int -> Benchmark
benchInsertExecuteMany conn n = bench "InsertExecuteMany" $ nfIO $ do
  stmt <- prepare conn "INSERT INTO testInsert VALUES(?, ?, ?)"
  executeMany stmt $ map toTestInsert [1 .. n]
  commit conn

benchInsertRun :: IConnection conn => conn -> Int -> Benchmark
benchInsertRun conn n = bench "InsertRun" $ nfIO $ do
  mapM_ (run conn "INSERT INTO testInsert VALUES (?, ?, ?)") $ map toTestInsert [1 .. n]
  commit conn
