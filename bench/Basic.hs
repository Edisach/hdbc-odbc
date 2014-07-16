module Basic(benchBasic, setupBasic, teardownBasic) where 

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
              , benchSelectExecuteMany conn n
              , benchInsertRun conn
              , benchInsertRunBind conn
              , benchInsertExecute conn
              , benchInsertExecuteBind conn
              , benchInsertExecuteMany conn
              , benchInsertExecuteManyBind conn
              ]

-- Utilities
setupBasic :: IConnection conn => conn -> IO ()
setupBasic conn = do
  run conn "DROP TABLE IF EXISTS testBasic" []
  run conn "CREATE TABLE testBasic (char CHAR(10))" []
  commit conn

teardownBasic :: IConnection conn => conn -> IO ()
teardownBasic conn = do
  run conn "DROP TABLE testBasic" []
  commit conn

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

benchInsertRun conn = bench "InsertRun 1" $ nfIO $ do
  run conn "INSERT INTO testBasic VALUES ('1')" []
  commit conn

benchInsertRunBind conn = bench "InsertRun Bind" $ nfIO $ do
  run conn "INSERT INTO testBasic VALUES (?)" [toSql "1"]
  commit conn

benchInsertExecute conn = bench "InsertExecute 1" $ nfIO $ do
  sth <- prepare conn "INSERT INTO testBasic VALUES ('1')"
  execute sth []
  commit conn

benchInsertExecuteBind conn = bench "InsertExecute Bind" $ nfIO $ do
  sth <- prepare conn "INSERT INTO testBasic VALUES (?)"
  execute sth [toSql "1"]
  commit conn

benchInsertExecuteMany conn = bench "InsertExecuteMany 1" $ nfIO $ do
  sth <- prepare conn "INSERT INTO testBasic VALUES ('2')"
  executeMany sth []
  commit conn

benchInsertExecuteManyBind conn = bench "InsertExecuteMany Bind" $ nfIO $ do
  sth <- prepare conn "INSERT INTO testBasic VALUES (?)"
  executeMany sth [[toSql "3"]]
  commit conn
