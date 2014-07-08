module Types(benchTypes) where

import Criterion.Main (Benchmark, bench, whnf, bgroup)
import Database.HDBC

-- Benchmark group
benchTypes = bgroup "Types" 
             [ benchToSql
             , benchIToSql
             , benchNToSql
             , benchFromSql
             --, benchSafeFromSql 
             ]

-- Utilities

-- Benchmarks
benchToSql = bench "toSql" $ whnf toSql "1"
benchIToSql = bench "iToSql" $ whnf iToSql 1
benchNToSql = bench "nToSql" $ whnf nToSql 1
benchFromSql = bench "fromSql" $ whnf (fromSql::SqlValue -> Int) (SqlInt64 1)
