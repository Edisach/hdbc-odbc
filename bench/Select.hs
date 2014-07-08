module Select(setupSelect, teardownSelect, benchSelect) where

import Criterion.Main (Benchmark, bench, nfIO, bgroup)
import Database.HDBC
import Control.Monad (forM, mapM, mapM_)

-- The benchmark group
benchSelect conn n = bgroup "Select"
              [ benchSelectQuickQuery' conn n
              , benchSelectQuickQuery conn n
              , benchSelectExecute conn n ]

-- Utilities
setupSelect :: IConnection conn => conn -> Int -> IO ()
setupSelect conn n = do
  run conn "DROP TABLE IF EXISTS testSelect" []
  run conn
    "CREATE TABLE testSelect (char CHAR(10), int INTEGER, float FLOAT)" []
  commit conn
  stmt <- prepare conn "INSERT INTO testSelect VALUES (?, ?, ?)"
  executeMany stmt $ map toTestSelect (replicate n 1)
  commit conn

teardownSelect :: IConnection conn => conn -> IO()
teardownSelect conn = do
  run conn "DROP TABLE testSelect" []
  commit conn

toTestSelect :: Int -> [SqlValue]
toTestSelect x = [ SqlString (show x)
  , SqlInt32 (fromIntegral x)
  , SqlDouble (fromIntegral x)]

-- The benchmarks
benchSelectQuickQuery' :: IConnection conn => conn -> Int -> Benchmark
benchSelectQuickQuery' conn n = bench "SelectQuickQuery'" $ nfIO $ do
  vs <- quickQuery' conn 
    "SELECT int FROM testSelect LIMIT ?" [iToSql n]
  commit conn
  if ((sum . map (\[v] -> fromSql v :: Int)) vs == n)
    then return ()
    else error "benchSelectQuickQuery': Unexpected sum!"

benchSelectQuickQuery conn n = bench "SelectQuickQuery" $ nfIO $ do
  vs <- quickQuery conn
    "SELECT int FROM testSelect LIMIT ?" [iToSql n]
  commit conn
  if ((sum . map (\[v] -> fromSql v :: Int)) vs == n)
    then return ()
    else error "benchSelectQuickQuery: Unexpected sum!"

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


