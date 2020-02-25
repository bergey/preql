{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
-- | SQL Effect class, basically capturing some way of accessing a database.

module Preql.Effect
    ( Query, SQL(..)
    ) where

import           Preql.Wire

import           Control.Exception (throwIO)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Control.Monad.Trans.State (StateT)
import           Control.Monad.Trans.Writer (WriterT)
import           Data.Vector (Vector)
import           Database.PostgreSQL.LibPQ (Connection)

import qualified Preql.Wire.Query as W

-- | An Effect class for running SQL queries.  You can think of this
-- as a context specifying a particular Postgres connection (or connection
-- pool).
class Monad m => SQL (m :: * -> *) where
    query :: (ToSql p, FromSql r) => (Query, p) -> m (Vector r)
    query_ :: FromSql r => Query -> m (Vector r)
    execute :: ToSql p => (Query, p) -> m ()

    default query :: (MonadTrans t, SQL m', m ~ t m', ToSql p, FromSql r) => (Query, p) -> m (Vector r)
    query qp = lift (query qp)

    default query_ :: (MonadTrans t, SQL m', m ~ t m', FromSql r) => Query -> m (Vector r)
    query_ = lift . query_

    default execute :: (MonadTrans t, SQL m', m ~ t m', ToSql p) => (Query, p) -> m ()
    execute qp = lift (execute qp)

-- | Most larger applications will define an instance; this one is suitable to test out the library.
instance SQL (ReaderT Connection IO) where
    query (q, p) = do
        conn <- ask
        lift (either throwIO pure =<< W.runQuery conn q p)
    query_ q = do
        conn <- ask
        lift (either throwIO pure =<< W.runQuery conn q ())
    execute (q, p) = do
        conn <- ask
        lift (either throwIO pure =<< W.runQuery_ conn q p)

instance SQL m => SQL (ExceptT e m)
instance SQL m => SQL (MaybeT m)
instance SQL m => SQL (StateT s m)
instance (Monoid w, SQL m) => SQL (WriterT w m)
instance {-# OVERLAPPABLE #-} SQL m => SQL (ReaderT r m)
