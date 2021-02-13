{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Main where

import           Preql
import qualified Preql.Wire.TypeInfo.Static  as OID
import Preql.FromSql
import Preql.FromSql.TH

import           Control.Concurrent          (forkIO, forkOS, myThreadId,
                                              threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.DeepSeq             (NFData (..), force)
import           Control.Exception           (evaluate, throwIO)
import           Control.Monad
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Reader  (ReaderT (..), ask, runReaderT)
import           Data.ByteString             (ByteString)
import           Data.Foldable               (for_)
import           Data.Int
import           Data.Pool
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Time                   (diffUTCTime, getCurrentTime)
import           Data.Time.Format.ISO8601    (iso8601Show)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import qualified Database.PostgreSQL.LibPQ   as PQ
import           Debug.Trace                 (traceEventIO)
import           GHC.Generics
import           Options.Applicative
import qualified PostgreSQL.Binary.Decoding  as PGB
import           System.Environment          (lookupEnv)
import           System.Exit                 (exitSuccess)
import           System.IO

main :: IO ()
main = do
    Options {..} <- execParser (info (options <**> helper) fullDesc)
    m_pool :: Maybe (Pool PQ.Connection) <- traverse (createPool connectDB PQ.finish 1 1) connections
    startTime <- getCurrentTime
    lastPrintTime <- newTVarIO startTime
    lastPrintCount <- newTVarIO 0
    rowCount <- newTVarIO 0
    hSetBuffering stdout LineBuffering
    replicateM_ threads $ forkIO $ do
        let
            typmod = -1 :: Int16
            isdefined = True
            selectRows :: PQ.Connection -> IO ()
            selectRows conn = do
                traceEventIO "before query"
                res :: Vector TypeInfo <-
                    flip runReaderT conn $ query [sql| select typname, typnamespace, typowner, typlen, typbyval , typcategory, typispreferred, typisdefined, typdelim , typrelid, typelem, typarray from pg_type where typtypmod = ${typmod} and typisdefined = ${isdefined} |]
                traceEventIO "after query"
                evaluate $ force res
                traceEventIO "forced rows"
                atomically $ modifyTVar' rowCount (+ V.length res)
        case m_pool of
            Just pool -> forever (withResource pool selectRows)
            Nothing   -> do
                conn <- connectDB
                forever (selectRows conn)

    let repeatedly = case duration of
            Nothing      -> forever
            Just seconds -> replicateM_ seconds
    repeatedly $ do
        threadDelay 1_000_000
        now <- getCurrentTime
        m_print <- atomically $ do
            rows <- readTVar rowCount
            last <- swapTVar lastPrintCount rows
            lastTime <- swapTVar lastPrintTime now
            let dt = floor (now `diffUTCTime` lastTime)
            return $ Just (rows, (rows - last) `div` dt)
        for_ m_print $ \(rows, rps) -> putStrLn (iso8601Show now ++ ": " ++ show rows ++ " total " ++ show rps ++ " per second")

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
            Just s  -> encodeUtf8 (T.pack s)
            Nothing -> "preql_tests"
    return $ "dbname=" <> dbname

data Options = Options
    { threads     :: Int
    , connections :: Maybe Int
    , duration    :: Maybe Int
    }
    deriving (Show)

options :: Parser Options
options = do
    threads <- option auto
        ( long "threads" <> short 't' <> metavar "THREADS"
          <> help "number of Haskell threads to fork" <> value 1)
    connections <- optional (option auto
        ( long "connections" <> short 'c' <>
          help "number of connections in pool (default no pool, 1 connection per thread)" ))
    duration <- optional $ option auto
        ( long "duration" <> short 'd' <> metavar "SECONDS"
          <> help "duration in seconds to run (default until killed)" )
    return Options {..}

instance NFData PQ.Oid where
    rnf (PQ.Oid oid) = rnf oid
instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8, NFData a9, NFData a10, NFData a11, NFData a12) =>
         NFData (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  rnf (x1,x2,x3,x4,x5,x6, x7, x8, x9, x10, x11, x12) =
      rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5
      `seq` rnf x6 `seq` rnf x7 `seq` rnf x8 `seq` rnf x9 `seq` rnf x10
      `seq` rnf x11 `seq` rnf x12

deriving newtype instance NFData PgName

data TypeInfo = TypeInfo
    { typname        :: !PgName
    , typnamespace   :: !PQ.Oid
    , typowner       :: !PQ.Oid
    , typlen         :: !Int16
    , typbyval       :: !Bool
    , typcategory    :: !Char
    , typispreferred :: !Bool
    , typisdefined   :: !Bool
    , typdelim       :: !Char
    , typrelid       :: !PQ.Oid
    , typelem        :: !PQ.Oid
    , typarray       :: !PQ.Oid
    }
    deriving (Generic, NFData)
$(deriveFromSql ''TypeInfo)
