{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | SQL Effect class, basically capturing some way of accessing a database.

module Preql.Effect
    ( module Preql.Effect, Transaction
    ) where

import Preql.Effect.Internal
import Preql.Imports
import Preql.Wire

import Control.Exception (throwIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Database.PostgreSQL.LibPQ (Connection)

import qualified Preql.Wire.Query as W


-- | An Effect class for running SQL queries.  You can think of this as a context
-- specifying a particular Postgres connection (or connection pool).  A minimal instance
-- defines @runTransaction@.  A typical instance will use 'runTransactionIO' or functions
-- in 'Preql.Wire.Query' and log & rethrow errors.
class SqlQuery m => SQL (m :: * -> *) where
    -- | Run multiple queries in a transaction.
    runTransaction' :: IsolationLevel -> Transaction a -> m a

-- | Run a Transaction with full Serializable isolation.
runTransaction :: SQL m => Transaction a -> m a
runTransaction = runTransaction' Serializable

-- | SqlQuery is separate from 'SQL' so that nested 'Transaction's are
-- statically prevented.  @query@ can be used directly within any
-- 'SQL' monad (running a single-statement transaction), or within a
-- 'Transaction'.
class Monad m => SqlQuery (m :: * -> *) where
    -- | Run a parameterized query that returns data.  The tuple argument is typically provided by
    -- the 'sql' Quasiquoter.
    query :: (ToSql p, FromSql r) => (Query, p) -> m (Vector r)
    default query :: (ToSql p, FromSql r, SQL m) => (Query, p) -> m (Vector r)
    query = runTransaction . query

    -- | Run a parameterized query that does not return data.
    query_ :: ToSql p => (Query, p) -> m ()
    default query_ :: (ToSql p, SQL m) => (Query, p) -> m ()
    query_ = runTransaction . query_

-- | Most larger applications will define an instance; this one is suitable to test out the library.
instance SqlQuery (ReaderT Connection IO)
instance SQL (ReaderT Connection IO) where
    runTransaction' level t = do
        conn <- ask
        lift (either throwIO pure =<< runTransactionIO level t conn)

-- | Lift through any monad transformer without a more specific instance.
instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad (t m), SqlQuery m) => SqlQuery (t m) where
    query = lift . query
    query_ = lift . query_

instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad (t m), SQL m) => SQL (t m) where
    runTransaction' level t = lift (runTransaction' level t)

-- * Transactions

-- | Run the provided 'Transaction'.  If it fails with a 'QueryError', roll back.
runTransactionIO :: IsolationLevel -> Transaction a -> Connection -> IO (Either QueryError a)
runTransactionIO level (Transaction m) conn = do
    either throwIO pure =<< W.begin conn level
    e_a <- runReaderT (runExceptT m) conn
    void $ case e_a of
        Left _ -> W.rollback conn
        Right _ -> W.commit conn
    return e_a

-- | The same @query@ methods can be used within a @Transaction@.
-- Nested @Transactions@ are implemented using savepoints.
instance SqlQuery Transaction where
    query (q, p) = Transaction (ExceptT (ReaderT (\conn -> W.query conn q p)))
    query_ (q, p) = Transaction (ExceptT (ReaderT (\conn -> W.query_ conn q p)))
