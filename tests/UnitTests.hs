module UnitTests(hTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception
import qualified Data.Map as Map

import Database.HDBC
import Database.HDBC.ODBC
import Database.HDBC.ODBC.Statement
import Database.HDBC.ODBC.Connection
import Database.HDBC.ODBC.ConnectionImpl(Connection(getIconn))

import Utils

dbInfo = dbTest $ (\dbh ->
      do putStrLn "----------"
         putStrLn $ "Driver name: " ++ hdbcDriverName dbh
         putStrLn $ "Client: " ++ hdbcClientVer dbh
         putStrLn $ "Proxy driver: " ++ proxiedClientName dbh
         putStrLn $ "Proxy client: " ++ proxiedClientVer dbh
         putStrLn $ "Server version: " ++ dbServerVer dbh
         putStrLn "----------"
         )

openCloseDB = handleSqlError $ 
      do dbh <- connectDB
         disconnect dbh

multiFinish = dbTest $ (\dbh ->
      do sth <- prepare dbh "SELECT 2"
         r <- execute sth []
         commit dbh
         assertEqual "count 0" 0 r
         finish sth
         finish sth
         finish sth
         )

basicQuery = dbTest $ (\dbh ->
      do sth <- prepare dbh "SELECT 2"
         s <- execute sth []
         commit dbh
         assertEqual "count 0" 0 s
         r <- fetchAllRows sth
         assertEqual "fromSql" [["2"]] (map (map fromSql) r)
         assertEqual "safeFromSql" [[Right "2"]] (map (map safeFromSql) r)
         assertEqual "int32" [[SqlInt32 2]] r
         assertEqual "iToSql" [[iToSql 2]] r
         assertEqual "toSql" [[toSql (2::Int)]] r
         assertEqual "nToSql" [[nToSql (2::Int)]] r
         assertEqual "string" [[SqlString "2"]] r
         )

createTable = dbTest $ (\dbh ->
      do run dbh "DROP TABLE IF EXISTS test1" []
         commit dbh
         run dbh "CREATE TABLE test1 (name VARCHAR(10), value INTEGER)" []
         commit dbh
         )

dropTable = dbTest $ (\dbh ->
      do run dbh "DROP TABLE test1" []
         run dbh "DROP TABLE test2" []
         commit dbh
         )

testRun = dbTest $ (\dbh ->
      do run dbh "INSERT INTO test1 VALUES ('run', ?)" [iToSql 1]
         commit dbh
         )

testQuickQuery = dbTest $ (\dbh ->
      do vss <- quickQuery dbh "SELECT * FROM test1 WHERE name = 'run'" []
         commit dbh
         assertEqual "equality" [[toSql "run", iToSql 1]] vss
         )

testQuickQuery' = dbTest $ (\dbh ->
      do vss <- quickQuery' dbh "SELECT * FROM test1 WHERE name = 'run'" []
         commit dbh
         assertEqual "equality" [[toSql "run", iToSql 1]] vss
         )

testRunRaw = dbTest $ (\dbh ->
      do runRaw dbh "INSERT INTO test1 VALUES ('runRaw', 2)"
         commit dbh
         vss <- quickQuery' dbh "SELECT value FROM test1 where name = 'runRaw'" []
         commit dbh
         assertEqual "equality" [[iToSql 2]] vss
         )

testPrepare = dbTest $ (\dbh ->
      do sth <- prepare dbh "SELECT * FROM test1"
         return ()
         )

testExecute = dbTest $ (\dbh -> 
      do sth <- prepare dbh "INSERT INTO test1 VALUES ('execute', ?)"
         qry <- prepare dbh "SELECT value FROM test1 WHERE name = 'execute'"
         execute sth [iToSql 1]
         execute qry []
         commit dbh
         fetchAllRows qry >>= assertEqual "equality" ([[iToSql 1]])
         return ()
         )

testExecuteRaw = dbTest $ (\dbh ->
      do sth <- prepare dbh "INSERT INTO test1 VALUES ('executeRaw', 3)"
         qry <- prepare dbh "SELECT value FROM test1 WHERE name = 'executeRaw'"
         executeRaw sth
         execute qry []
         commit dbh
         fetchAllRows qry >>= assertEqual "equality" ([[iToSql 1]])
         return ()
         )

testSExecute = dbTest $ (\dbh ->
      do sth <- prepare dbh "INSERT INTO test1 VALUES ('sExecute', ?)"
         qry <- prepare dbh "SELECT value FROM test1 WHERE name = 'sExecute'"
         sExecute sth [Just "1"]
         sExecute qry []
         commit dbh
         fetchAllRows qry >>= assertEqual "equality" ([[iToSql 1]])
         return ()
         )

testExecuteMany = dbTest $ (\dbh ->
      do sth <- prepare dbh "INSERT INTO test1 VALUES ('executeMany', ?)"
         qry <- prepare dbh "SELECT value FROM test1 WHERE name='executeMany'"
         executeMany sth rows
         execute qry []
         commit dbh
         fetchAllRows qry >>= assertEqual "equality" rows
         return()
         )
           where rows = map (\x -> [iToSql x]) [1 .. 9]

testSExecuteMany = dbTest $ (\dbh ->
      do sth <- prepare dbh "INSERT INTO test1 VALUES ('sExecuteMany', ?)"
         qry <- prepare dbh "SELECT value FROM test1 WHERE name='sExecuteMany'"
         sExecuteMany sth rows
         execute qry []
         commit dbh
         fetchAllRows qry >>= assertEqual "equality" checkrows
         return()
         )
           where rows = map (\x -> [Just (show x)]) [1 .. 9]
                 checkrows = map (\x -> [iToSql x]) [1 .. 9]

testRollback = dbTest $ (\dbh ->
      do assertBool "Database does not support transactions" (dbTransactionSupport dbh)
         sth <- prepare dbh "INSERT INTO test1 VALUES ('rollback', ?)"
         qry <- prepare dbh "SELECT value FROM test1 WHERE name = 'rollback'"
         execute sth [iToSql 1]
         execute qry []
         commit dbh
         fetchAllRows qry >>= (assertEqual "first commit" [[iToSql 1]])
         execute sth [iToSql 5]
         rollback dbh
         fetchAllRows qry >>= (assertEqual "rollback" [])
         )

testWithTransaction = dbTest$ (\dbh ->
      do assertBool "Database does not support transactions" (dbTransactionSupport dbh)
         sth <- prepare dbh "INSERT INTO test1 VALUES ('trans', ?)"
         qry <- prepare dbh "SELECT value FROM test1 WHERE name = 'trans'"
         execute sth [iToSql 1]
         commit dbh
         execute qry []
         commit dbh
         fetchAllRows qry >>= assertEqual "commit" [[iToSql 1]]
         -- deliberately fail 
         catch (withTransaction dbh (\_ -> do execute qry [iToSql 2]
                                              fail "woops"))
               (\e -> do return (e::IOException)
                         return ())
         execute qry []
         commit dbh
         fetchAllRows qry >>= assertEqual "deliberate fail" [[iToSql 1]]
         -- now try succesfully
         withTransaction dbh (\_ -> do execute sth [iToSql 2]
                                       commit dbh)
         execute qry []
         commit dbh
         fetchAllRows qry >>= assertEqual "success" [[iToSql 1], [iToSql 2]]
         )

testGetTables = dbTest $ (\dbh ->
      do tables <- getTables dbh
         commit dbh
         assertBool "getTables" ("test1" `elem` tables)
         )

testDescribeTable = dbTest $ (\dbh ->
      do table <- describeTable dbh "test1"
         commit dbh
         assertEqual "describeTable" ["name", "value"] (map fst table)
         )

testSRun = dbTest $ (\dbh ->
      do a <- sRun dbh "INSERT INTO test1 VALUES ('sRun1', ?)" [Just "3"]
         b <- sRun dbh "INSERT INTO test1 VALUES ('sRun2', ?)" [Nothing]
         commit dbh
         assertEqual "one row entered" a 1
         assertEqual "another row entered" b 1
         yss <- quickQuery' dbh "SELECT value FROM test1 WHERE name='sRun2'" []
         vss <- quickQuery' dbh "SELECT value FROM test1 WHERE name='sRun1'" []
         commit dbh
         assertEqual "'yes'" [[iToSql 3]] vss
         assertEqual "null" [[SqlNull]] yss
         )

testOriginalQuery = dbTest $ (\dbh ->
      do sth <- prepare dbh "SELECT ?"
         assertEqual "equality" (originalQuery sth) "SELECT ?"
         )

testNewSState = dbTest $ \dbh -> 
  do iconn <- getIconn dbh
     state1 <- newSState iconn "SELECT 1"
     state2 <- newSState iconn "SELECT 2"
     return ()

rawRowData = ["fetch", "1"]
rowdata = map toSql rawRowData
cols = ["name", "value"]
alRows = zip cols rowdata
mapRows = Map.fromList alRows
justRows = map (\x -> Just x) rawRowData

setupFetch = dbTest $ (\dbh -> 
      do run dbh "DROP TABLE IF EXISTS test2" []
         commit dbh
         run dbh "CREATE TABLE test2 (name VARCHAR(10), value VARCHAR(20))" []
         commit dbh
         run dbh "INSERT INTO test2 VALUES (?, ?)" rowdata
         commit dbh
         )

testFetchRow = dbTest $ (\dbh ->
      do qry <- prepare dbh "SELECT * FROM test2"
         execute qry []
         commit dbh
         fetchRow qry >>= assertEqual "one row" (Just rowdata)
         fetchRow qry >>= assertEqual "no more rows" (Nothing)
         )

testFetchRowAL = dbTest $ (\dbh ->
      do run dbh "INSERT INTO test1 VALUES ('fetchRowAL', 1)" []
         commit dbh
         qry <- prepare dbh "SELECT * FROM test2"
         execute qry []
         commit dbh
         fetchRowAL qry >>= assertEqual "one row" (Just alRows)
         fetchRowAL qry >>= assertEqual "no more row" Nothing
         )

testFetchRowMap = dbTest $ (\dbh ->
      do qry <- prepare dbh "SELECT * FROM test2"
         execute qry []
         commit dbh
         fetchRowMap qry >>= assertEqual "one row" (Just mapRows)
         fetchRowMap qry >>= assertEqual "no more rows" Nothing
         )

testSFetchRow = dbTest $ (\dbh ->
      do qry <- prepare dbh "SELECT * FROM test2"
         execute qry []
         commit dbh
         sFetchRow qry >>= assertEqual "one row" (Just justRows)
         sFetchRow qry >>= assertEqual "no more rows" Nothing
         )

testFetchAllRows = dbTest $ (\dbh ->
      do qry <- prepare dbh "SELECT * FROM test2"
         execute qry []
         commit dbh
         fetchAllRows qry >>= assertEqual "one row" [rowdata]
         )

testFetchAllRows' = dbTest $ (\dbh ->
      do qry <- prepare dbh "SELECT * FROM test2"
         execute qry []
         commit dbh
         fetchAllRows' qry >>= assertEqual "one row" [rowdata]
         )

testFetchAllRowsAL = dbTest $ (\dbh ->
      do qry <- prepare dbh "SELECT * FROM test2"
         execute qry []
         commit dbh
         fetchAllRowsAL qry >>= assertEqual "one row" [alRows]
         )

testFetchAllRowsAL' = dbTest $ (\dbh ->
      do qry <- prepare dbh "SELECT * FROM test2"
         execute qry []
         commit dbh
         fetchAllRowsAL' qry >>= assertEqual "one row" [alRows]
         )

testFetchAllRowsMap = dbTest $ (\dbh ->
      do qry <- prepare dbh "SELECT * FROM test2"
         execute qry []
         commit dbh
         fetchAllRowsMap qry >>= assertEqual "one row" [mapRows]
         )

testFetchAllRowsMap' = dbTest $ (\dbh ->
      do qry <- prepare dbh "SELECT * FROM test2"
         execute qry []
         commit dbh
         fetchAllRowsMap' qry >>= assertEqual "one row" [mapRows]
         )

testSFetchAllRows = dbTest $ (\dbh ->
      do qry <- prepare dbh "SELECT * FROM test2"
         execute qry []
         commit dbh 
         sFetchAllRows qry >>= assertEqual "one row" [justRows]
         )

testSFetchAllRows' = dbTest $ (\dbh ->
      do qry <- prepare dbh "SELECT * FROM test2"
         execute qry []
         commit dbh
         sFetchAllRows' qry >>= assertEqual "one row" [justRows]
         )

fetchTests = testGroup "Fetch tests" 
         [ testCase "setup fetch" setupFetch
         , testCase "fetchRow" testFetchRow
         , testCase "fetchRowAL" testFetchRowAL
         , testCase "fetchRowMap" testFetchRowMap 
         , testCase "sFetchRow" testSFetchRow
         , testCase "fetchAllRows" testFetchAllRows
         , testCase "fetchAllRows'" testFetchAllRows'
         , testCase "fetchAllRowsAL" testFetchAllRowsAL
         , testCase "fetchAllRowsAL'" testFetchAllRowsAL'
         , testCase "fetchAllRowsMap" testFetchAllRowsMap
         , testCase "fetchAllRowsMap'" testFetchAllRowsMap'
         , testCase "sFetchAllRows" testSFetchAllRows
         , testCase "sFetchAllRows'" testSFetchAllRows'
         ]

hTests = testGroup "HUnit Tests" 
         --[ testCase "Print info" dbInfo
         [ testCase "Open and close DB" openCloseDB
         , testCase "Multiple finish" multiFinish
         , testCase "Basic conversions" basicQuery
         , testCase "Create table" createTable
         , testCase "run, insert, bindParam" testRun
         , testCase "quickQuery" testQuickQuery
         , testCase "quickQuery'" testQuickQuery'
         , testCase "runRaw" testRunRaw
         , testCase "prepare" testPrepare
         , testCase "execute" testExecute
         , testCase "sExecute" testSExecute
         --, testCase "executeRaw" testExecuteRaw
         , testCase "executeMany" testExecuteMany
         , testCase "sExecuteMany" testSExecuteMany
         , testCase "rollback" testRollback
         , testCase "withTransaction" testWithTransaction
         , testCase "getTables" testGetTables
         , testCase "describeTable" testDescribeTable
         , testCase "sRun" testSRun
         , testCase "originalQuery" testOriginalQuery
         , fetchTests
         --, testCase "Drop table" dropTable
         ]
