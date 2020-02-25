{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
-- | SQL Effect class, basically capturing some way of accessing a database.

module Preql.SqlEffect
    ( Query, SQL(..)
    ) where

import            Preql.Untyped.Query (Query(..))

import           Data.Int (Int64)

import           Database.PostgreSQL.Simple (Connection, FromRow,  ToRow)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Control.Monad.Trans.State (StateT)
import           Control.Monad.Trans.Writer (WriterT)
import           Control.Monad.Trans.Class (MonadTrans(..))

import qualified Preql.Untyped.Query as TQ

-- | An Effect class for running SQL queries.  You can think of this
-- as a context specifying a particular Postgres connection (or connection
-- pool).
class Monad m => SQL (m :: * -> *) where
    query :: (ToRow p, FromRow r) => (Query p r , p) -> m [r]
    query_ :: FromRow r => Query () r -> m [r]
    execute :: ToRow p => (Query p (), p) -> m Int64

    default query :: (MonadTrans t, SQL m', m ~ t m', ToRow p, FromRow r) => (Query p r, p) -> m [r]
    query qp = lift (query qp)

    default query_ :: (MonadTrans t, SQL m', m ~ t m', FromRow r) => Query () r -> m [r]
    query_ = lift . query_

    default execute :: (MonadTrans t, SQL m', m ~ t m', ToRow p) => (Query p (), p) -> m Int64
    execute qp = lift (execute qp)

-- | Most larger applications will define an instance; this one is suitable to test out the library.
instance SQL (ReaderT Connection IO) where
    query qp = do
        conn <- ask
        lift $ TQ.query conn qp
    query_ q = do
        conn <- ask
        lift $ TQ.query_ conn q
    execute qp = do
        conn <- ask
        lift $ TQ.execute conn qp

instance SQL m => SQL (ExceptT e m)
instance SQL m => SQL (MaybeT m)
instance SQL m => SQL (StateT s m)
instance (Monoid w, SQL m) => SQL (WriterT w m)
instance {-# OVERLAPPABLE #-} SQL m => SQL (ReaderT r m)
