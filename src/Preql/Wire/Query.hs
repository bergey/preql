{-# LANGUAGE FlexibleContexts #-}
module Preql.Wire.Query where

import Preql.FromSql
import Preql.Wire.Decode
import Preql.Wire.Errors
import Preql.Wire.Internal
import Preql.Wire.ToSql

import Control.Monad
import GHC.TypeNats
import Preql.Imports

import Debug.Trace

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Database.PostgreSQL.LibPQ as PQ

queryWith :: KnownNat (Width r) =>
  RowEncoder p -> RowDecoder (Width r) r -> PQ.Connection ->
  Query (Width r) -> p -> IO (Either QueryError (Vector r))
queryWith enc dec conn (Query q) params = do
    -- TODO safer Connection type
    -- withMVar (connectionHandle conn) $ \connRaw -> do
        e_result <- execParams enc conn q params
        traceEventIO "execParams > decodeVector"
        case e_result of
            Left err   -> return (Left err)
            Right rows -> decodeVector (lookupType conn) dec rows

-- If there is no result, we don't need a Decoder
queryWith_ :: RowEncoder p -> PQ.Connection -> Query n -> p -> IO (Either QueryError ())
queryWith_ enc conn (Query q) params = do
    e_result <- execParams enc conn q params
    return (void e_result)

query :: (ToSql p, FromSql r, KnownNat (Width r)) =>
    PQ.Connection -> Query (Width r) -> p -> IO (Either QueryError (Vector r))
query = queryWith toSql fromSql

query_ :: ToSql p => PQ.Connection -> Query n -> p -> IO (Either QueryError ())
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

lookupType :: PQ.Connection -> PgType -> IO (Either QueryError PQ.Oid)
lookupType _ (Oid oid _) = return (Right oid)
lookupType conn (TypeName name) = do
    e_rows <- query conn "SELECT oid FROM pg_type WHERE typname = $1" name
    case fmap (V.!? 0) e_rows of
        Left e -> return (Left e)
        Right (Just oid) -> return (Right oid)
        Right Nothing -> return (Left (ConnectionError ("No oid for: " <> name)))

data IsolationLevel = ReadCommitted
    | RepeatableRead
    | Serializable
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

begin :: PQ.Connection -> IsolationLevel -> IO (Either QueryError ())
begin conn level = query_ conn q () where
  q = case level of
    ReadCommitted  -> "BEGIN ISOLATION LEVEL READ COMMITTED"
    RepeatableRead -> "BEGIN ISOLATION LEVEL REPEATABLE READ"
    Serializable   -> "BEGIN ISOLATION LEVEL SERIALIZABLE"

commit :: PQ.Connection -> IO (Either QueryError ())
commit conn = query_ conn "COMMIT" ()

rollback :: PQ.Connection -> IO (Either QueryError ())
rollback conn = query_ conn "ROLLBACK" ()
