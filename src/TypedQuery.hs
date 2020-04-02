{-# LANGUAGE ScopedTypeVariables #-}

module TypedQuery where

import Preql.Wire as W
import Preql.Wire.Internal as W (Query(..))
import Untyped.Printer
import Untyped.Syntax as S (Query)
import qualified Preql.Wire.Query as W

import Control.Exception (throwIO)
import Data.Vector (Vector)
import qualified Database.PostgreSQL.LibPQ as PQ


-- | An SQL query, annotated with the types of its parameters and results.
data TypedQuery p r = TypedQuery String S.Query
    deriving Show

-- | compare @query conn q p@
query :: (ToSql p, FromSql r) => PQ.Connection -> (TypedQuery p r, p) -> IO (Vector r)
query conn (TypedQuery _raw parsed, p) = do
    let formatted = formatQuery parsed
    result <- W.query conn formatted p
    either throwIO pure result

-- | compare @query_ conn q p@
query_ :: ToSql p => PQ.Connection -> (TypedQuery p (), p) -> IO ()
query_ conn (TypedQuery _raw parsed, p) = do
    result <- W.query_ conn (formatQuery parsed) p
    either throwIO pure result

formatQuery :: S.Query -> W.Query
formatQuery = Query . formatAsByteString
