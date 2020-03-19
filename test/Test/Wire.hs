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
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Preql.Wire.Query as W

connectionString :: IO ByteString
connectionString = do
    m_dbname <- lookupEnv "PREQL_TESTS_DB"
    let dbname = case m_dbname of
            Just s -> encodeUtf8 (T.pack s)
            Nothing -> "preql_tests"
    return $ "host=localhost user=postgres dbname=" <> dbname

wire :: TestTree
wire = withResource (PQ.connectdb =<< connectionString) PQ.finish $ \db -> testGroup "wire" $
    let
        query :: (ToSql p, FromSql r) => Query -> p -> IO (Either W.QueryError (Vector r))
        query q p = db >>= \conn -> W.query conn q p
        query_ :: (ToSql p) => Query -> p -> IO ()
        query_ q p = db >>= \conn -> W.query_ conn q p >>= either throwIO pure
        assertQuery :: (FromSql r, Eq r, Show r) => Vector r -> Query -> IO ()
        assertQuery expected q = assertEqual "" (Right expected) =<< query q () in
        [ testGroup "decoders"
          [ testCase "decode True" $
            assertQuery [True] "SELECT true"
          , testCase "decode False" $
              assertQuery [False] "SELECT false"
          , testCase "decode Int64 literal" $
              assertQuery [2^32 :: Int64] "SELECT (2^32)::int8"
          , testCase "decode Int32 literal" $
              assertQuery [2^16 :: Int32] "SELECT (2^16)::int4"
          , testCase "decode Int16 literal" $
              assertQuery [2^8 :: Int16] "SELECT (2^8)::int2"
          , testCase "decode Float literal" $
              assertQuery [2**32 :: Float] "SELECT (2^32)::float4"
          , testCase "decode Double literal" $
              assertQuery [2**32 :: Double] "SELECT (2^32)::float8"
          -- , testCase "decode Char literal" $
          --     assertQuery ['x'] "SELECT 'x'::char"
          , testCase "decode String literal" $
              assertQuery ["foo" :: String] "SELECT 'foo'::text"
          , testCase "decode Text literal" $
              assertQuery ["foo" :: Text] "SELECT 'foo'::text"
          , testCase "decode lazy Text literal" $
              assertQuery ["foo" :: TL.Text] "SELECT 'foo'::text"
          , testCase "decode byte array literal" $
              assertQuery ["foo" :: ByteString] "SELECT 'foo'::bytea"
          , testCase "decode UTCTime literal" $ do
                  time <- iso8601ParseM "2020-03-19T21:43:00Z"
                  assertQuery [time :: UTCTime] "SELECT '2020-03-19T21:43:00Z'::timestamptz"
          , testCase "decode Day literal" $ do
                  time <- iso8601ParseM "2020-03-19"
                  assertQuery [time :: Day] "SELECT '2020-03-19'::date"
          , testCase "decode time literal" $ do
                  time <- iso8601ParseM "21:43:00"
                  assertQuery [time :: TimeOfDay] "SELECT '21:43:00Z'::time"
          , testCase "decode TimeTZ literal" $ do
                  time <- iso8601ParseM "21:43:00"
                  zone <- iso8601ParseM "+05:00"
                  assertQuery [TimeTZ time zone] "SELECT '21:43:00+05:00'::timetz"
          , testCase "decode UUID literal" $
              assertQuery [UUID.fromWords 0x01234567 0x89abcdef 0x01234567 0x89abcdef] "SELECT '01234567-89ab-cdef-0123-456789abcdef'::uuid"
          ]
        , testGroup "encoders"
            [ testCase "encode True" $ do
                query_ "CREATE TABLE IF NOT EXISTS encoder_tests (b boolean, i16 int2, i32 int4, i64 int8, f float4, d float8, t text)" ()
                query_ "truncate encoder_tests" ()
                query_ "INSERT INTO encoder_tests (b) VALUES (true)" ()
                assertQuery [True] "SELECT b FROM encoder_tests WHERE b is not null"
            , testCase "encode Int64" $ do
                query_ "INSERT INTO encoder_tests (i64) VALUES (12345)" ()
                assertQuery [12345 :: Int64] "SELECT i64 FROM encoder_tests WHERE i64 is not null"
            , testCase "encode Int32" $ do
                query_ "INSERT INTO encoder_tests (i32) VALUES (12345)" ()
                assertQuery [12345 :: Int32] "SELECT i32 FROM encoder_tests WHERE i32 is not null"
            , testCase "encode Int16" $ do
                query_ "INSERT INTO encoder_tests (i16) VALUES (12345)" ()
                assertQuery [12345 :: Int16] "SELECT i16 FROM encoder_tests WHERE i16 is not null"
            , testCase "encode Float" $ do
                query_ "INSERT INTO encoder_tests (f) VALUES (12345.678)" ()
                assertQuery [12345.678 :: Float] "SELECT f FROM encoder_tests WHERE f is not null"
            , testCase "encode Double" $ do
                query_ "INSERT INTO encoder_tests (d) VALUES (12345.678)" ()
                assertQuery [12345.678 :: Double] "SELECT d FROM encoder_tests WHERE d is not null"
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
