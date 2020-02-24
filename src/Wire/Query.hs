{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module Wire.Query where

import Imports
-- import           Wire.Connection
import           Wire.FromSql
import           Wire.ToSql

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.String (IsString)

import qualified Database.PostgreSQL.LibPQ as PQ

-- | A @Query@ is a string ready to be passed to Postgres, with
-- phantom type parameters describing its parameters and result.
-- Depending how the @Query@ was constructed, these parameters may be
-- inferred from context (offering no added type safety), or be
-- partly synthesized from the underlying string.
--
-- The IsString instance does no validation; the limited instances
-- discourage directly manipulating strings, with the high risk of SQL
-- injection.
newtype Query params result = Query ByteString
    deriving (Show, IsString)

data QueryAndParams params result = Q (Query params result) params

runQueryWith :: RowEncoder p -> RowDecoder r -> PQ.Connection -> Query p r -> p -> IO (Either QueryError (Vector r))
runQueryWith enc dec conn (Query query) params = runExceptT $ do
    -- TODO safer Connection type
    -- withMVar (connectionHandle conn) $ \connRaw -> do
        result <- queryError conn =<< liftIO (PQ.execParams conn query (runEncoder enc params) PQ.Binary)
        withExceptT DecoderError (decodeVector dec result)

-- If there is no result, we don't need a Decoder
runQueryWith_ :: RowEncoder p -> PQ.Connection -> Query p () -> p -> IO (Either QueryError ())
runQueryWith_ enc conn (Query query) params = runExceptT $ do
    result <- queryError conn =<< liftIO (PQ.execParams conn query (runEncoder enc params) PQ.Binary)
    status <- liftIO (PQ.resultStatus result)
    unless (status == PQ.CommandOk || status == PQ.TuplesOk) $ do
        msg <- liftIO (PQ.resStatus status)
        throwE (QueryError (decodeUtf8With lenientDecode msg))

runQuery :: (ToSql p, FromSql r) => PQ.Connection -> Query p r -> p -> IO (Either QueryError (Vector r))
runQuery = runQueryWith toSql fromSql

runQuery_ :: ToSql p => PQ.Connection -> Query p () -> p -> IO (Either QueryError ())
runQuery_ = runQueryWith_ toSql

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
