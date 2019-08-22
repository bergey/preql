{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module Query where

import           FromSql
import           ToSql
import           Connection

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans.Except
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

runQueryWith :: SqlEncoder p -> SqlDecoder r -> Connection -> Query p r -> p -> IO (Either String [r])
runQueryWith enc dec conn (Query query) params =
    withMVar (connectionHandle conn) $ \connRaw -> do
        -- TODO promote Nothing to a suitable error
        Just result <- PQ.execParams connRaw query (runEncoder enc params) PQ.Text
        ntuples <- PQ.ntuples result
        ok <- checkTypes dec result
        if ok
            then runExceptT $ traverse (runDecoder dec result) [0 .. ntuples - 1]
            else return (Left "Types do not match")

runQuery :: (ToSql p, FromSql r) => Connection -> Query p r -> p -> IO (Either String [r])
runQuery = runQueryWith toSql fromSql
