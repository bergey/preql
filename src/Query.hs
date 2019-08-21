{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module Query where

import           FromSql
import           Connection

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.ByteString (ByteString)
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

emptyValue :: Maybe a -> Either String a
emptyValue = maybe (Left "Got empty value string from Postgres") Right

-- TODO type class variant
-- TODO handle params
-- TODO handle multiple columns
runQuery :: Connection -> SqlDecoder r -> Query () r -> IO (Either String r)
runQuery conn dec (Query query) =
    withMVar (connectionHandle conn) $ \connRaw -> do
        Just result <- PQ.exec connRaw query
        (parse dec <=< emptyValue) <$> PQ.getvalue result (PQ.Row 0) (PQ.Col 0)
