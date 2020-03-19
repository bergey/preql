{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.Wire where

import Preql.Effect
import Preql.QuasiQuoter.Raw.TH (sql)
import Preql.Wire

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Either
import Data.Int
import Data.Text (Text)
import Data.Vector (Vector)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text.Lazy as TL
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Preql.Wire.Query as W

connectionString :: ByteString
connectionString = "host=localhost user=postgres dbname=preql_tests"

wire :: TestTree
wire = withResource (PQ.connectdb connectionString) PQ.finish $ \db -> testGroup "wire" $
    let
        query :: (ToSql p, FromSql r) => Query -> p -> IO (Either W.QueryError (Vector r))
        query q p = db >>= \conn -> W.query conn q p
        query_ :: (ToSql p) => Query -> p -> IO ()
        query_ q p = db >>= \conn -> W.query_ conn q p >>= either throwIO pure in
        [ testGroup "decoders"
          [ testCase "decode True" $
            assertEqual "" (Right [True]) =<< query "SELECT true" ()
          , testCase "decode False" $
              assertEqual "" (Right [False]) =<< query "SELECT false" ()
          , testCase "decode Int64 literal" $
              assertEqual "" (Right [2^32 :: Int64]) =<< query "SELECT (2^32)::int8" ()
          , testCase "decode Int32 literal" $
              assertEqual "" (Right [2^16 :: Int32]) =<< query "SELECT (2^16)::int4" ()
          , testCase "decode Int16 literal" $
              assertEqual "" (Right [2^8 :: Int16]) =<< query "SELECT (2^8)::int2" ()
          , testCase "decode Float literal" $
              assertEqual "" (Right [2**32 :: Float]) =<< query "SELECT (2^32)::float4" ()
          , testCase "decode Double literal" $
              assertEqual "" (Right [2**32 :: Double]) =<< query "SELECT (2^32)::float8" ()
          -- , testCase "decode Char literal" $
          --     assertEqual "" (Right ['x']) =<< query "SELECT 'x'::char" ()
          , testCase "decode String literal" $
              assertEqual "" (Right ["foo" :: String]) =<< query "SELECT 'foo'::text" ()
          , testCase "decode Text literal" $
              assertEqual "" (Right ["foo" :: Text]) =<< query "SELECT 'foo'::text" ()
          , testCase "decode lazy Text literal" $
              assertEqual "" (Right ["foo" :: TL.Text]) =<< query "SELECT 'foo'::text" ()
          -- TODO more field decoder tests
          ]
        , testGroup "encoders"
            [ testCase "encode True" $ do
                query_ "CREATE TABLE IF NOT EXISTS encoder_tests (b boolean, i16 int2, i32 int4, i64 int8, f float4, d float8, t text)" ()
                query_ "truncate encoder_tests" ()
                query_ "INSERT INTO encoder_tests (b) VALUES (true)" ()
                assertEqual "" (Right [True]) =<< uncurry query [sql| SELECT b FROM encoder_tests WHERE b is not null |]
            , testCase "encode Int64" $ do
                query_ "INSERT INTO encoder_tests (i64) VALUES (12345)" ()
                assertEqual "" (Right [12345 :: Int64]) =<< uncurry query [sql| SELECT i64 FROM encoder_tests WHERE i64 is not null |]
            , testCase "encode Int32" $ do
                query_ "INSERT INTO encoder_tests (i32) VALUES (12345)" ()
                assertEqual "" (Right [12345 :: Int32]) =<< uncurry query [sql| SELECT i32 FROM encoder_tests WHERE i32 is not null |]
            , testCase "encode Int16" $ do
                query_ "INSERT INTO encoder_tests (i16) VALUES (12345)" ()
                assertEqual "" (Right [12345 :: Int16]) =<< uncurry query [sql| SELECT i16 FROM encoder_tests WHERE i16 is not null |]
            , testCase "encode Float" $ do
                query_ "INSERT INTO encoder_tests (f) VALUES (12345.678)" ()
                assertEqual "" (Right [12345.678 :: Float]) =<< uncurry query [sql| SELECT f FROM encoder_tests WHERE f is not null |]
            , testCase "encode Double" $ do
                query_ "INSERT INTO encoder_tests (d) VALUES (12345.678)" ()
                assertEqual "" (Right [12345.678 :: Double]) =<< uncurry query [sql| SELECT d FROM encoder_tests WHERE d is not null |]
            ]
        , testGroup "parameters & expressions"
            [ testCase "schema type mismatch causes runtime error" $
                assertBool "" . isLeft =<< query @() @Int32 "SELECT 2.5" ()
            , testCase "ignore later columns" $
                assertEqual "" (Right [2]) =<< query @() @Int32 "SELECT 2, 3" ()
            , testCase "parse a pair" $
                assertEqual "" (Right [(2, 3)]) =<< query @() @(Int32, Int32) "SELECT 2, 3" ()
            , testCase "add Int32 parameters" $
                assertEqual "" (Right [3]) =<< query @(Int32, Int32) @Int32 "SELECT $1 + $2" (1, 2)
            , testCase "add Int64 parameters" $
                assertEqual "" (Right [3]) =<< query @(Int64, Int64) @Int64 "SELECT $1 + $2" (1, 2)
            , testCase "add Float32 parameters" $
                assertEqual "" (Right [3]) =<< query @(Float, Float) @Float "SELECT $1 + $2" (1, 2)
            , testCase "add Float64 parameters" $
                assertEqual "" (Right [3]) =<< query @(Double, Double) @Double "SELECT $1 + $2" (1, 2)
            , testCase "add 3 Int32 parameters" $
                assertEqual "" (Right [6]) =<< query @(Int32, Int32, Int32) @Int32 "SELECT $1 + $2 + $3" (1, 2, 3)
            ]
        ]

--     [ testCase "SELECT integer literal, raw PQ" $ do
--         conn <- connect database
--         connRaw <- takeMVar (connectionHandle conn)
--         Just result <- PQ.exec connRaw "SELECT 2;"
--         assertEqual "ntuples" (PQ.Row 1) =<< PQ.ntuples result
--         assertEqual "nfields" (PQ.Col 1) =<< PQ.nfields result
--         assertEqual "oid" (PQ.Oid 23) =<< PQ.ftype result (PQ.Col 0) -- int4
--         assertEqual "value" (Just "2") =<< PQ.getvalue result (PQ.Row 0) (PQ.Col 0)
--     , testCase "query" $ do
--         conn <- connect database
--         assertEqual "" (Right [2]) =<< query @() @Int32 conn  "SELECT 2" ()
--     ]
