module Utils(connectDB, dbTest) where

import Database.HDBC
import Database.HDBC.ODBC
import Database.HDBC.ODBC.Statement
import Database.HDBC.ODBC.ConnectionImpl(Connection(getIconn))
import Database.HDBC.ODBC.Connection

import Control.Exception

-- Get a connection to the test database
connectDB = handleSqlError $ connectODBC "DSN=HDBC-test"

-- Connect to the database, run x and definitely disconnect
dbTest x = 
  bracket (connectDB) (handleSqlError . disconnect) (handleSqlError . x)

