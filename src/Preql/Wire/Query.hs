module Preql.Wire.Query where

import Preql.Wire.FromSql
import Preql.Wire.Internal
import Preql.Wire.ToSql

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Except
import Preql.Imports

import qualified Data.Text as T
import qualified Database.PostgreSQL.LibPQ as PQ

queryWith :: RowEncoder p -> RowDecoder r -> PQ.Connection -> Query -> p -> IO (Either QueryError (Vector r))
queryWith enc dec conn (Query query) params = runExceptT $ do
    -- TODO safer Connection type
    -- withMVar (connectionHandle conn) $ \connRaw -> do
        result <- execParams enc conn query params
        withExceptT DecoderError (decodeVector dec result)

-- If there is no result, we don't need a Decoder
queryWith_ :: RowEncoder p -> PQ.Connection -> Query -> p -> IO (Either QueryError ())
queryWith_ enc conn (Query query) params =
    runExceptT (void (execParams enc conn query params))

execParams :: RowEncoder p -> PQ.Connection -> ByteString -> p -> ExceptT QueryError IO PQ.Result
execParams enc conn query params = do
    result <- queryError conn =<< liftIO (PQ.execParams conn query (runEncoder enc params) PQ.Binary)
    status <- liftIO (PQ.resultStatus result)
    unless (status == PQ.CommandOk || status == PQ.TuplesOk) $ do
        msg <- liftIO (PQ.resultErrorMessage result)
            <&> maybe (T.pack (show status)) (decodeUtf8With lenientDecode)
        throwE (QueryError msg)
    return result

query :: (ToSql p, FromSql r) => PQ.Connection -> Query -> p -> IO (Either QueryError (Vector r))
query = queryWith toSql fromSql

query_ :: ToSql p => PQ.Connection -> Query -> p -> IO (Either QueryError ())
query_ = queryWith_ toSql

data QueryError = QueryError Text | DecoderError DecoderError
    deriving (Eq, Show, Typeable)
instance Exception QueryError

queryError :: PQ.Connection -> Maybe a -> ExceptT QueryError IO a
queryError _conn (Just a) = return a
queryError conn Nothing = do
    m_msg <- liftIO $ PQ.errorMessage conn
    case m_msg of
        Just msg -> throwE (QueryError (decodeUtf8With lenientDecode msg))
        Nothing -> throwE (QueryError "No error message available")
