{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Preql
import qualified Preql.Wire.TypeInfo.Static as OID

import Control.Concurrent.STM.TVar
import Control.DeepSeq (NFData(..), force)
import Control.Exception (evaluate, throwIO)
import Control.Monad
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Int
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector (Vector)
import System.Environment (lookupEnv)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQL.Binary.Decoding as PGB

main :: IO ()
main = do
    startTime <- getCurrentTime
    lastPrintTime <- newTVarIO startTime
    lastPrintCount <- newTVarIO 0
    rowCount <- newTVarIO 0
    -- TODO make connection count configurable
    replicateM_ 10 $ do
        conn <- connectDB
        forever $ do
            let
                typmod = -1 :: Int16
                isdefined = True
            res :: Vector (PgName, PQ.Oid, PQ.Oid, Int16, Bool , Char, Bool, Bool, Char , PQ.Oid, PQ.Oid, PQ.Oid) <-
                flip runReaderT conn $ query [sql| select typname, typnamespace, typowner, typlen, typbyval , typcategory, typispreferred, typisdefined, typdelim , typrelid, typelem, typarray from pg_type where typtypmod = ${typmod} and typisdefined = ${isdefined} |]
            evaluate $ force res
            now <- getCurrentTime
            m_print <- atomically $ do
                modifyTVar' rowCount (+ V.length res)
                last <- readTVar lastPrintTime
                if now `diffUTCTime` last > 5 -- seconds
                then do
                    writeTVar lastPrintTime now
                    rows <- readTVar rowCount
                    last <- swapTVar lastPrintCount rows
                    return $ Just (rows, (rows - last) `div` 5)
                else return Nothing
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
