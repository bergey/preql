{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Wire where

import Preql.Wire
import Test.Wire.Enum

import Control.Exception (Exception, bracket_, throwIO)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Either
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Vector (Vector)
import GHC.TypeNats
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Time.Format.ISO8601 (iso8601ParseM)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQL.Binary.Decoding as PGB
import qualified Preql.Wire.Query as W

wire :: TestTree
wire = withResource initDB PQ.finish $ \db -> testGroup "wire" $
    let
        inTransaction desc body = testCase desc $
            bracket_ (query_ "BEGIN TRANSACTION" ()) (query_ "ROLLBACK" ()) body
        query :: (ToSql p, FromSql r, KnownNat (Width r)) => Query (Width r) -> p -> IO (Either QueryError (Vector r))
        query q p = db >>= \conn -> W.query conn q p
        query_ :: (ToSql p) => Query 0 -> p -> IO ()
        query_ q p = db >>= \conn -> W.query_ conn q p >>= either throwIO pure
        assertQuery :: (FromSql r, Eq r, Show r, KnownNat (Width r)) => Vector r -> Query (Width r) -> IO ()
        assertQuery expected q = assertEqual "" (Right expected) =<< query q () in
        [ testGroup "decoders"
          [ inTransaction "decode True" $
            assertQuery [True] "SELECT true"
          , inTransaction "decode False" $
              assertQuery [False] "SELECT false"
          , inTransaction "decode Int64 literal" $
              assertQuery [2 `expt` 32 :: Int64] "SELECT (2^32)::int8"
          , inTransaction "decode Int32 literal" $
              assertQuery [2 `expt` 16 :: Int32] "SELECT (2^16)::int4"
          , inTransaction "decode Int16 literal" $
              assertQuery [2 `expt` 8 :: Int16] "SELECT (2^8)::int2"
          , inTransaction "decode Float literal" $
              assertQuery [2**32 :: Float] "SELECT (2 ^ 32)::float4"
          , inTransaction "decode Double literal" $
              assertQuery [2**32 :: Double] "SELECT (2^32)::float8"
          -- , inTransaction "decode Char literal" $
          --     assertQuery ['x'] "SELECT 'x'::char"
          , inTransaction "decode String literal" $
              assertQuery ["foo" :: String] "SELECT 'foo'::text"
          , inTransaction "decode Text literal" $
              assertQuery ["foo" :: Text] "SELECT 'foo'::text"
          , inTransaction "decode lazy Text literal" $
              assertQuery ["foo" :: TL.Text] "SELECT 'foo'::text"
          , inTransaction "decode byte array literal" $
              assertQuery ["foo" :: ByteString] "SELECT 'foo'::bytea"
          , inTransaction "decode UTCTime literal" $ do
                  time <- iso8601ParseM "2020-03-19T21:43:00Z"
                  assertQuery [time :: UTCTime] "SELECT '2020-03-19T21:43:00Z'::timestamptz"
          , inTransaction "decode Day literal" $ do
                  time <- iso8601ParseM "2020-03-19"
                  assertQuery [time :: Day] "SELECT '2020-03-19'::date"
          , inTransaction "decode time literal" $ do
                  time <- iso8601ParseM "21:43:00"
                  assertQuery [time :: TimeOfDay] "SELECT '21:43:00Z'::time"
          , inTransaction "decode TimeTZ literal" $ do
                  time <- iso8601ParseM "21:43:00"
                  zone <- iso8601ParseM "+05:00"
                  assertQuery [TimeTZ time zone] "SELECT '21:43:00+05:00'::timetz"
          , inTransaction "decode UUID literal" $
              assertQuery [UUID.fromWords 0x01234567 0x89abcdef 0x01234567 0x89abcdef] "SELECT '01234567-89ab-cdef-0123-456789abcdef'::uuid"
          ]

        , testGroup "encoders"
            [ inTransaction "encode True" $ do
                query_ "INSERT INTO encoder_tests (b) VALUES ($1)" True
                assertQuery [True] "SELECT b FROM encoder_tests WHERE b is not null"
            , inTransaction "encode Int64" $ do
                let v = 12345 :: Int64
                query_ "INSERT INTO encoder_tests (i64) VALUES ($1)" v
                assertQuery [v] "SELECT i64 FROM encoder_tests WHERE i64 is not null"
            , inTransaction "encode Int32" $ do
                let v = 12345 :: Int32
                query_ "INSERT INTO encoder_tests (i32) VALUES ($1)" v
                assertQuery [v] "SELECT i32 FROM encoder_tests WHERE i32 is not null"
            , inTransaction "encode Int16" $ do
                let v = 12345 :: Int16
                query_ "INSERT INTO encoder_tests (i16) VALUES ($1)" v
                assertQuery [v] "SELECT i16 FROM encoder_tests WHERE i16 is not null"
            , inTransaction "encode Float" $ do
                let v = 12345.678 :: Float
                query_ "INSERT INTO encoder_tests (f) VALUES ($1)" v
                assertQuery [v] "SELECT f FROM encoder_tests WHERE f is not null"
            , inTransaction "encode Double" $ do
                let v = 12345.678 :: Double
                query_ "INSERT INTO encoder_tests (d) VALUES ($1)" v
                assertQuery [v] "SELECT d FROM encoder_tests WHERE d is not null"
            , inTransaction "encode strict Text" $ do
                let v = "foo" :: Text
                query_ "INSERT INTO encoder_tests (t) VALUES ($1)" v
                assertQuery [v] "SELECT t FROM encoder_tests WHERE t is not null"
            , inTransaction "encode lazy Text" $ do
                let v = "foo" :: TL.Text
                query_ "INSERT INTO encoder_tests (t) VALUES ($1)" v
                assertQuery [v] "SELECT t FROM encoder_tests WHERE t is not null"
            , inTransaction "encode String" $ do
                let v = "foo" :: String
                query_ "INSERT INTO encoder_tests (t) VALUES ($1)" v
                assertQuery [v] "SELECT t FROM encoder_tests WHERE t is not null"
            , inTransaction "encode UTCTime" $ do
                v :: UTCTime <- iso8601ParseM "2020-03-19T21:43:00Z"
                query_ "INSERT INTO encoder_tests (ts) VALUES ($1)" v
                assertQuery [v] "SELECT ts FROM encoder_tests WHERE ts is not null"
            , inTransaction "encode Day" $ do
                v :: Day <- iso8601ParseM "2020-03-19"
                query_ "INSERT INTO encoder_tests (day) VALUES ($1)" v
                assertQuery [v] "SELECT day FROM encoder_tests WHERE day is not null"
            , inTransaction "encode TimeOfDay" $ do
                v :: TimeOfDay <- iso8601ParseM "21:43:00"
                query_ "INSERT INTO encoder_tests (time) VALUES ($1)" v
                assertQuery [v] "SELECT time FROM encoder_tests WHERE time is not null"
            , inTransaction "encode TimeTZ" $ do
                v <- TimeTZ <$> iso8601ParseM "21:43:00" <*> iso8601ParseM "+05:00"
                query_ "INSERT INTO encoder_tests (ttz) VALUES ($1)" v
                assertQuery [v] "SELECT ttz FROM encoder_tests WHERE ttz is not null"
          , inTransaction "encode UUID" $ do
                let v = UUID.fromWords 0x01234567 0x89abcdef 0x01234567 0x89abcdef
                query_ "INSERT INTO encoder_tests (u) VALUES ($1)" v
                assertQuery [v] "SELECT u FROM encoder_tests WHERE u is not null"
            ]

        , testGroup "parameters & expressions"
            [ inTransaction "schema type mismatch causes runtime error" $
                assertBool "" . isLeft =<< query @() @Int32 "SELECT 2.5" ()
            , inTransaction "ignore later columns" $
                assertEqual "" (Right [2]) =<< query @() @Int32 "SELECT 2, 3" ()
            , inTransaction "parse a pair" $
                assertEqual "" (Right [(2, 3)]) =<< query @() @(Int32, Int32) "SELECT 2, 3" ()
            , inTransaction "add Int32 parameters" $
                assertEqual "" (Right [3]) =<< query @(Int32, Int32) @Int32 "SELECT $1 + $2" (1, 2)
            , inTransaction "add Int64 parameters" $
                assertEqual "" (Right [3]) =<< query @(Int64, Int64) @Int64 "SELECT $1 + $2" (1, 2)
            , inTransaction "add Float32 parameters" $
                assertEqual "" (Right [3]) =<< query @(Float, Float) @Float "SELECT $1 + $2" (1, 2)
            , inTransaction "add Float64 parameters" $
                assertEqual "" (Right [3]) =<< query @(Double, Double) @Double "SELECT $1 + $2" (1, 2)
            , inTransaction "add 3 Int32 parameters" $
                assertEqual "" (Right [6]) =<< query @(Int32, Int32, Int32) @Int32 "SELECT $1 + $2 + $3" (1, 2, 3)
            ]

        , testGroup "TH-derived instances"
            [ inTransaction "FromSql 4-tuple" $
                assertQuery [(1 :: Int64, 2 :: Int64, 3 :: Int64, 4 :: Int64)] "SELECT 1::int8, 2::int8, 3::int8, 4::int8 "
            , inTransaction "ToSql 4-tuple" $ do
                let v = (True, 2 :: Int16, 3 :: Int32, 4 :: Int64)
                query_ "INSERT INTO encoder_tests (b, i16, i32, i64) VALUES ($1, $2, $3, $4)" v
                assertQuery [v] "SELECT b, i16, i32, i64 FROM encoder_tests"
            ]

        , testGroup "user-defined types"
          [ inTransaction "decode enum" $ do
                  query_ "drop type if exists my_enum" ()
                  query_ "create type my_enum as enum  ('A', 'B', 'C')" ()
                  assertQuery [A] "select 'A'::my_enum"
          , inTransaction "type mismatch error" $ do
                  mismatch <- query @() @MyEnum "select 'A'" ()
                  case mismatch of
                      Left (PgTypeMismatch
                             [TypeMismatch { expected = TypeName "my_enum", column = 0 }]) -> pure ()
                      _ -> assertFailure "did not get expected TypeMismatch error"
          , inTransaction "composite type: complex" $ do
              query_ "drop type if exists complex" ()
              query_ "create type complex as (r float, i float)" ()
              result <- query "select row(-1,0)::complex as v" ()
              assertEqual "" (Right [Complex (-1) 0]) result
          , inTransaction "composite type: (bool, int)" $ do
              query_ "drop type if exists foo" ()
              query_ "create type foo as (bar bool, baz int)" ()
              result <- query "select row(true, 1)::foo as foo" ()
              assertEqual "" (Right [Foo True 1]) result
          ]
        ]

initDB :: HasCallStack => IO PQ.Connection
initDB = do
    conn <- PQ.connectdb =<< connectionString
    status <- PQ.status conn
    unless (status == PQ.ConnectionOk) (throwIO =<< badConnection conn)
    void $ W.query_ conn "DROP TABLE IF EXISTS encoder_tests" ()
    void $ W.query_ conn "CREATE TABLE encoder_tests (b boolean, i16 int2, i32 int4, i64 int8, f float4, d float8, t text, by bytea, ts timestamptz, day date, time time, ttz timetz, u uuid)" ()
    return conn

connectionString :: IO ByteString
connectionString = do
    m_dbname <- lookupEnv "PREQL_TESTS_DB"
    let dbname = case m_dbname of
            Just s  -> encodeUtf8 (T.pack s)
            Nothing -> "preql_tests"
    return $ "dbname=" <> dbname

data BadConnection = BadConnection
    { status       :: PQ.ConnStatus
    , errorMessage :: ByteString
    , host         :: ByteString
    , port         :: ByteString
    , user         :: ByteString
    }
    deriving (Show)
instance Exception BadConnection

badConnection :: PQ.Connection -> IO BadConnection
badConnection c = do
    status <- PQ.status c
    errorMessage <- fromMaybe "" <$> PQ.errorMessage c
    host <- fromMaybe "" <$> PQ.host c
    port <- fromMaybe "" <$> PQ.port c
    user <- fromMaybe "" <$> PQ.user c
    return BadConnection {..}

data Complex = Complex { real :: !Double, imag :: !Double } deriving (Show, Eq)

instance FromSql Complex where
  fromSql = notNull (FieldDecoder (TypeName "complex") (PGB.composite (Complex <$> PGB.valueComposite PGB.float8 <*> PGB.valueComposite PGB.float8)))

data Foo = Foo !Bool !Int deriving (Show, Eq)

instance FromSql Foo where
  fromSql = notNull (FieldDecoder (TypeName "foo") (PGB.composite (Foo <$> PGB.valueComposite PGB.bool <*> PGB.valueComposite PGB.int)))

expt :: Num a => a -> Int64 -> a
expt = (^)
