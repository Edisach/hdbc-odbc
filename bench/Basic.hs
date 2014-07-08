module Basic(benchBasic) where 

import Criterion.Main (Benchmark, bench, nfIO, bgroup)
import Database.HDBC
import Control.Monad (forM, mapM, mapM_)

-- The benchmark group
benchBasic conn n = bgroup "Basic"
              [ benchSelectExecute1 conn
              , benchSelectRun1 conn
              , benchQuickQuery conn
              , benchQuickQuery' conn
              , benchSelectExecute conn n
              , benchSelectExecuteMany conn n]

-- Utilities

-- Benchmarks

benchSelectRun1 conn = bench "SelectRun 1" $ nfIO $ do
  run conn "SELECT 1" []
  commit conn

benchSelectExecute1 conn = bench "SelectExecute 1" $ nfIO $ do
  stmt <- prepare conn "SELECT 1"
  execute stmt []
  commit conn
  
benchSelectExecute conn n = bench "SelectExecute" $ nfIO $ do
  stmt <- prepare conn "SELECT ?"
  mapM (execute stmt) $ map (\x -> [iToSql x]) [1 .. n]
  commit conn

benchSelectExecuteMany conn n = bench "SelectExecuteMany" $ nfIO $ do
  stmt <- prepare conn "SELECT ?"
  executeMany stmt (map (\x -> [iToSql x]) [1 .. n])
  commit conn

benchQuickQuery conn = bench "SelectQuickQuery 1" $ nfIO $ do
  quickQuery conn "SELECT 1" []
  commit conn

benchQuickQuery' conn = bench "SelectQuickQuery' 1" $ nfIO $ do
  quickQuery' conn "SELECT 1" []
  commit conn


