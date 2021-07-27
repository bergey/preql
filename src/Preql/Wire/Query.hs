-- | Description: Send queries, decode results, look up OID for a known type name

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
module Preql.Wire.Query where

import Preql.FromSql
import Preql.Wire.Errors
import Preql.Wire.Internal
import Preql.Wire.ToSql

import Control.Exception (try)
import Control.Monad
import Control.Monad.Except
import Data.IORef
import GHC.TypeNats
import Preql.Imports
import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import qualified Database.PostgreSQL.LibPQ as PQ

-- Cache of OIDs by type name

type TypeCache = IORef (HM.HashMap Text PQ.Oid)

-- | We make the type cache part of the Connection to offer the option of
-- per-Connection (or striped) caches.  It's also reasonable to share a single
-- cache for an entire multi-threaded program; the @IORef@ supports this usage.
data Connection = Connection
  { connection :: !PQ.Connection
  , typeCache :: !TypeCache
  }

connectdbSharedCache :: ByteString -> IO Connection
connectdbSharedCache str = Connection <$> PQ.connectdb str <*> pure globalCache

connectdbNewCache :: ByteString -> IO Connection
connectdbNewCache str = Connection <$> PQ.connectdb str <*> newIORef mempty

finish :: Connection -> IO ()
finish (Connection conn _) = PQ.finish conn

globalCache :: TypeCache
globalCache = unsafePerformIO (newIORef mempty)

-- send queries, receiving results

queryWith :: KnownNat (Width r) =>
  RowEncoder p -> RowDecoder (Width r) r -> Connection ->
  Query (Width r) -> p -> IO (Either QueryError (Vector r))
queryWith enc dec conn@(Connection pqConn cache) (Query q) params = do
  e_result <- execParams enc pqConn q params
  traceEventIO "execParams > decodeVector"
  case e_result of
    Left err   -> return (Left err)
    Right rows -> decodeVector conn dec rows

-- If there is no result, we don't need a Decoder
queryWith_ :: RowEncoder p -> Connection -> Query n -> p -> IO (Either QueryError ())
queryWith_ enc (Connection conn _) (Query q) params = do
    e_result <- execParams enc conn q params
    return (void e_result)

query :: (ToSql p, FromSql r, KnownNat (Width r)) =>
    Connection -> Query (Width r) -> p -> IO (Either QueryError (Vector r))
query = queryWith toSql fromSql

query_ :: ToSql p => Connection -> Query n -> p -> IO (Either QueryError ())
query_ = queryWith_ toSql

execParams :: RowEncoder p -> PQ.Connection -> ByteString -> p -> IO (Either QueryError PQ.Result)
execParams enc conn q params = do
    e_result <- connectionError conn =<< PQ.execParams conn q (runEncoder enc params) PQ.Binary
    case e_result of
        Left err -> return (Left (ConnectionError err))
        Right res -> do
            status <- PQ.resultStatus res
            if status == PQ.CommandOk || status == PQ.TuplesOk
                then return (Right res)
                else do
                    msg <- PQ.resultErrorMessage res
                        <&> maybe (T.pack (show status)) (decodeUtf8With lenientDecode)
                    return (Left (ConnectionError msg))

connectionError :: PQ.Connection -> Maybe a -> IO (Either Text a)
connectionError _conn (Just a) = return (Right a)
connectionError conn Nothing = do
    m_msg <- liftIO $ PQ.errorMessage conn
    case m_msg of
        Just msg -> return (Left (decodeUtf8With lenientDecode msg))
        Nothing  -> return (Left "No error message available")

-- decoding

decodeVector :: KnownNat n =>
  Connection -> RowDecoder n a -> PQ.Result -> IO (Either QueryError (Vector a))
decodeVector conn rd@(RowDecoder pgtypes _parsers) result = do
    mismatches <- fmap (catMaybes . VS.toList) $ for (VS.zip (VS.enumFromN 0) pgtypes) $ \(column@(PQ.Col cint), expected) -> do
        actual <- PQ.ftype result column
        lookupResult <- lookupType conn expected
        let mismatch = do
              m_name <- liftIO $ PQ.fname result column
              let columnName = decodeUtf8With lenientDecode <$> m_name
              return $ Just (TypeMismatch{column = fromIntegral cint, ..})
        case lookupResult of
          Cached oid | actual == oid -> return Nothing
          FromDb oid | actual == oid -> return Nothing
          Cached _ -> do -- recheck DB, in case type changed under us
            e_oid  <- lookupTypeIgnoreCache conn expected
            case e_oid of
              Right oid | actual == oid -> return Nothing
              _ -> mismatch
          _ -> mismatch
    if not (null mismatches)
        then return (Left (PgTypeMismatch mismatches))
        else do
            (PQ.Row ntuples) <- liftIO $ PQ.ntuples result
            ref <- newIORef (DecoderState result 0 0)
            fmap (first DecoderError) . try $
                V.replicateM (fromIntegral ntuples) (decodeRow ref rd result)
  where

lookupType :: Connection -> PgType -> IO LookupResult
lookupType _ (Oid oid _) = return (FromDb oid)
lookupType conn@(Connection _ cacheRef) expected@(TypeName name) = do
  cache <- readIORef cacheRef
  case HM.lookup name cache of
    Just oid -> return (Cached oid)
    Nothing -> lookupTypeIgnoreCache conn expected
      <&> either LookupError FromDb

data LookupResult
  = Cached PQ.Oid
  | FromDb PQ.Oid
  | LookupError QueryError

lookupTypeIgnoreCache :: Connection -> PgType -> IO (Either QueryError PQ.Oid)
lookupTypeIgnoreCache _ (Oid oid _) = return (Right oid)
lookupTypeIgnoreCache conn@(Connection _ cacheRef) (TypeName name) = do
  e_rows <- query conn "SELECT oid FROM pg_type WHERE typname = $1" name
  case fmap (V.!? 0) e_rows of
    Left e -> return (Left e)
    Right (Just oid) -> do
      atomicModifyIORef cacheRef (\cache -> (HM.insert name oid cache, ()))
      return (Right oid)
    Right Nothing -> return (Left (ConnectionError ("No oid for: " <> name)))

-- transactions

data IsolationLevel = ReadCommitted
    | RepeatableRead
    | Serializable
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

begin :: Connection -> IsolationLevel -> IO (Either QueryError ())
begin conn level = query_ conn q () where
  q = case level of
    ReadCommitted  -> "BEGIN ISOLATION LEVEL READ COMMITTED"
    RepeatableRead -> "BEGIN ISOLATION LEVEL REPEATABLE READ"
    Serializable   -> "BEGIN ISOLATION LEVEL SERIALIZABLE"

commit :: Connection -> IO (Either QueryError ())
commit conn = query_ conn "COMMIT" ()

rollback :: Connection -> IO (Either QueryError ())
rollback conn = query_ conn "ROLLBACK" ()
