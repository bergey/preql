module Preql.Wire.Query where

import Preql.Wire.Errors
import Preql.Wire.FromSql
import Preql.Wire.Internal
import Preql.Wire.ToSql

import Control.Monad
import Preql.Imports

import qualified Data.Text as T
import qualified Database.PostgreSQL.LibPQ as PQ

queryWith :: RowEncoder p -> RowDecoder r -> PQ.Connection -> Query -> p -> IO (Either QueryError (Vector r))
queryWith enc dec conn (Query query) params = do
    -- TODO safer Connection type
    -- withMVar (connectionHandle conn) $ \connRaw -> do
        e_result <- execParams enc conn query params
        case e_result of
            Left err -> return (Left err)
            Right result -> decodeVector dec result

-- If there is no result, we don't need a Decoder
queryWith_ :: RowEncoder p -> PQ.Connection -> Query -> p -> IO (Either QueryError ())
queryWith_ enc conn (Query query) params = do
    e_result <- execParams enc conn query params
    return (void e_result)

execParams :: RowEncoder p -> PQ.Connection -> ByteString -> p -> IO (Either QueryError PQ.Result)
execParams enc conn query params = do
    e_result <- connectionError conn =<< liftIO (PQ.execParams conn query (runEncoder enc params) PQ.Binary)
    case e_result of
        Left err -> return (Left (ConnectionError err))
        Right result -> do
            status <- liftIO (PQ.resultStatus result)
            if status == PQ.CommandOk || status == PQ.TuplesOk
                then do
                    msg <- liftIO (PQ.resultErrorMessage result)
                        <&> maybe (T.pack (show status)) (decodeUtf8With lenientDecode)
                    return (Left (ConnectionError msg))
                else return (Right result)

query :: (ToSql p, FromSql r) => PQ.Connection -> Query -> p -> IO (Either QueryError (Vector r))
query = queryWith toSql fromSql

query_ :: ToSql p => PQ.Connection -> Query -> p -> IO (Either QueryError ())
query_ = queryWith_ toSql

connectionError :: PQ.Connection -> Maybe a -> IO (Either Text a)
connectionError _conn (Just a) = return (Right a)
connectionError conn Nothing = do
    m_msg <- liftIO $ PQ.errorMessage conn
    case m_msg of
        Just msg -> return (Left (decodeUtf8With lenientDecode msg))
        Nothing -> return (Left "No error message available")
