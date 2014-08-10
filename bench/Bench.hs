module Main where

import Criterion.Main (Benchmark, bench, defaultMain, nfIO, bgroup)

import Database.HDBC
import Database.HDBC.ODBC

import Basic
import Insert
import Select
import Types

import Control.Monad (forM, mapM, mapM_)

main :: IO ()
main = do
  -- requires entry in odbc.ini with name "HDBC-test"
  conn <- connectODBC "DSN=HDBC-test"
  setupInsert conn
  setupSelect conn 10000
  setupBasic conn
  defaultMain 
    [ benchBasic conn 1000
    , benchTypes
    , benchInsert conn 1000
    , benchSelect conn 1000]
  teardownInsert conn
  teardownSelect conn
  teardownBasic conn
  disconnect conn





