{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Preql.QuasiQuoter.Syntax.TypedQuery where

import Preql.QuasiQuoter.Syntax.Printer
import Preql.QuasiQuoter.Syntax.Syntax as S (Query)
import Preql.Wire as W
import Preql.Wire.Internal as W (Query(..))
import qualified Preql.Wire.Query as W

import Control.Exception (throwIO)
import Data.Vector (Vector)
import GHC.TypeNats
import qualified Database.PostgreSQL.LibPQ as PQ


-- | An SQL query, annotated with the types of its parameters and results.
data TypedQuery p r = TypedQuery String S.Query
    deriving Show

-- | compare @query conn q p@
query :: (ToSql p, FromSql r, KnownNat (Width r)) =>
  PQ.Connection -> (TypedQuery p r, p) -> IO (Vector r)
query conn (TypedQuery _raw parsed, p) = do
    let formatted = formatQuery parsed
    result <- W.query conn formatted p
    either throwIO pure result

-- | compare @query_ conn q p@
query_ :: ToSql p => PQ.Connection -> (TypedQuery p (), p) -> IO ()
query_ conn (TypedQuery _raw parsed, p) = do
    result <- W.query_ conn (formatQuery parsed) p
    either throwIO pure result

-- TODO get rid of this or make it safe
formatQuery :: S.Query -> W.Query n
formatQuery = Query . formatAsByteString
