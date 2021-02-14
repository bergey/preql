{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Preql
import Preql.FromSql
import qualified Preql.Wire.TypeInfo.Static as OID

import Control.DeepSeq (NFData(..), deepseq)
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Control.Parallel (par)
import Criterion
import Criterion.Main
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import GHC.Generics
import System.Environment (lookupEnv)
import qualified Data.Text as T
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQL.Binary.Decoding as PGB

data Foo = Foo
  { email :: Text
  , age :: Int16
  , pets :: [Text]
  } deriving (Eq, Show, NFData, Generic)
  deriving anyclass (ToJSON, FromJSON)
instance FromSqlField Foo where fromSqlField = fromSqlJsonField
instance FromSql Foo

main :: IO ()
main = do
  conn <- connectDB
  let
    withConn :: ReaderT PQ.Connection IO a -> IO a
    withConn = flip runReaderT conn
  defaultMain
    [ bench "pg_type" $ nfIO $ withConn $ do
            let
                typmod = -1 :: Int16
                isdefined = True
            res <- query [sql| select typname, typnamespace, typowner, typlen, typbyval , typcategory, typispreferred, typisdefined, typdelim , typrelid, typelem, typarray from pg_type where typtypmod = ${typmod} and typisdefined = ${isdefined} |]
            return (res :: Vector (PgName, PQ.Oid, PQ.Oid, Int16, Bool
                                  , Char, Bool, Bool, Char
                                  , PQ.Oid, PQ.Oid, PQ.Oid))
    , bench "jsonb decode only" $ nf (decode @Foo)
      "{\"email\": \"bergey@teallabs.org\", \"age\": 36, \"pets\": [\"Marx\", \"Jones\"]}"
    , bench "jsonb baseline" $ nfIO $ withConn $ jsonbQuery
    , bench "jsonb skip decode" $ nfIO $ withConn $ void jsonbQuery
    , bench "jsonb parallel decode" $ nfIO $ withConn $ do
        r1 <- jsonbQuery
        r2 <- r1 `par` jsonbQuery
        return (r1, r2)
    ]

jsonbQuery :: ReaderT PQ.Connection IO (Vector Foo)
jsonbQuery = query [sql| select jsonb_build_object('email', 'bergey@teallabs.org', 'age', 36, 'pets', '{Marx, Jones}'::text[]) |]

connectDB :: IO PQ.Connection
connectDB = do
    conn <- PQ.connectdb =<< connectionString
    status <- PQ.status conn
    unless (status == PQ.ConnectionOk) (error "bad connection")
    return conn

connectionString :: IO ByteString
connectionString = do
    m_dbname <- lookupEnv "PREQL_TESTS_DB"
    let dbname = case m_dbname of
            Just s -> encodeUtf8 (T.pack s)
            Nothing -> "preql_tests"
    return $ "dbname=" <> dbname

instance NFData PQ.Oid where
    rnf (PQ.Oid oid) = rnf oid
instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8, NFData a9, NFData a10, NFData a11, NFData a12) =>
         NFData (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  rnf (x1,x2,x3,x4,x5,x6, x7, x8, x9, x10, x11, x12) =
      rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5
      `seq` rnf x6 `seq` rnf x7 `seq` rnf x8 `seq` rnf x9 `seq` rnf x10
      `seq` rnf x11 `seq` rnf x12

deriving newtype instance NFData PgName
