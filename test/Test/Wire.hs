{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.Wire where

import           Wire.Connection
import           Wire.FromSql
import           Wire.Query

import           Control.Concurrent.MVar
import           Data.Either
import           Data.Int
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Database.PostgreSQL.LibPQ as PQ

database :: ConnectInfo
database = ConnectInfo
    { connectHost = "localhost"
    , connectUser = "bergey"
    , connectDatabase = "crispy_broccoli"
    , connectPort = 5432
    , connectPassword = ""
    }

wire :: TestTree
wire = testGroup "wire"
    [ testCase "SELECT integer literal, raw PQ" $ do
        conn <- connect database
        connRaw <- takeMVar (connectionHandle conn)
        Just result <- PQ.exec connRaw "SELECT 2;"
        assertEqual "ntuples" (PQ.Row 1) =<< PQ.ntuples result
        assertEqual "nfields" (PQ.Col 1) =<< PQ.nfields result
        assertEqual "oid" (PQ.Oid 23) =<< PQ.ftype result (PQ.Col 0) -- int4
        assertEqual "value" (Just "2") =<< PQ.getvalue result (PQ.Row 0) (PQ.Col 0)
    , testCase "runQuery" $ do
        conn <- connect database
        assertEqual "" (Right [2]) =<< runQuery @() @Int32 conn  "SELECT 2" ()
    , testCase "schema type mismatch causes runtime error" $ do
        conn <- connect database
        assertBool "" . isLeft =<< runQuery @() @Int32 conn "SELECT 2.5" ()
    , testCase "ignore later columns" $ do
        conn <- connect database
        assertEqual "" (Right [2]) =<< runQuery @() @Int32 conn "SELECT 2, 3" ()
    , testCase "parse a pair" $ do
        conn <- connect database
        assertEqual "" (Right [(2, 3)]) =<< runQuery @() @(Int32, Int32) conn "SELECT 2, 3" ()
    , testCase "add Int32 parameters" $ do
        conn <- connect database
        assertEqual "" (Right [3]) =<< runQuery @(Int32, Int32) @Int32 conn "SELECT $1 + $2" (1, 2)
    , testCase "add Int64 parameters" $ do
        conn <- connect database
        assertEqual "" (Right [3]) =<< runQuery @(Int64, Int64) @Int64 conn "SELECT $1 + $2" (1, 2)
    , testCase "add Float32 parameters" $ do
        conn <- connect database
        assertEqual "" (Right [3]) =<< runQuery @(Float, Float) @Float conn "SELECT $1 + $2" (1, 2)
    , testCase "add Float64 parameters" $ do
        conn <- connect database
        assertEqual "" (Right [3]) =<< runQuery @(Double, Double) @Double conn "SELECT $1 + $2" (1, 2)
    , testCase "add 3 Int32 parameters" $ do
        conn <- connect database
        assertEqual "" (Right [6]) =<< runQuery @(Int32, Int32, Int32) @Int32 conn "SELECT $1 + $2 + $3" (1, 2, 3)
    ]
