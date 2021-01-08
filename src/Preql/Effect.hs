{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Description: SQL & SqlQuery classes
--
-- Effect class, expressing that a database connection is available or
-- can be acquired, and transactions run.

module Preql.Effect
    ( module Preql.Effect, Transaction
    ) where

import Preql.Effect.Internal
import Preql.Imports
import Preql.Wire

import Control.Exception (throwIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Database.PostgreSQL.LibPQ (Connection)
import GHC.TypeNats
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.State.Strict as SS

import qualified Preql.Wire.Query as W

-- | An Effect class for running SQL queries.  You can think of this as a context
-- specifying a particular Postgres connection (or connection pool).  A minimal instance
-- defines @withConnection@.
--
-- Override the remaining methods to log errors before rethrowing, or not to rethrow.
class SqlQuery m => SQL (m :: * -> *) where
    -- | Run multiple queries in a transaction.
    runTransaction' :: IsolationLevel -> Transaction a -> m a
    default runTransaction' :: MonadIO m => IsolationLevel -> Transaction a -> m a
    runTransaction' level t = withConnection $ \conn ->
        liftIO $ either throwIO pure =<< runTransactionIO level t conn

    -- | @runTransaction@ covers the most common patterns of
    -- mult-statement transactions.  @withConnection@ is useful when
    -- you want more control, or want to override the defaults that
    -- your instance defines.  For example:
    --      - change the number of retries
    --      - interleave calls to other services with the Postgres transaction
    --      - ensure a prepared statement is shared among successive transactions
    withConnection :: (Connection -> m a) -> m a

    -- | Run a query on the specified 'Connection'
    queryOn :: (ToSql p, FromSql r, KnownNat (Width r)) =>
        Connection -> (Query (Width r), p) -> m (Vector r)
    default queryOn :: (ToSql p, FromSql r, KnownNat (Width r), MonadIO m) =>
        Connection -> (Query (Width r), p) -> m (Vector r)
    queryOn conn (q, p) = liftIO $ either throwIO pure =<< W.query conn q p

    queryOn_ :: ToSql p => Connection -> (Query 0, p) -> m ()
    default queryOn_ :: (ToSql p, MonadIO m) => Connection -> (Query 0, p) -> m ()
    queryOn_ conn (q, p) = liftIO $ either throwIO pure =<< W.query_ conn q p

-- | Run a Transaction with full Serializable isolation.
runTransaction :: SQL m => Transaction a -> m a
runTransaction = runTransaction' Serializable

-- | SqlQuery is separate from 'SQL' so that nested 'Transaction's are
-- statically prevented.  @query@ can be used directly within any
-- 'SQL' monad (running a single-statement transaction), or within a
-- 'Transaction'.
--
-- Users should not need to define instances, as every @SQL@ instance
-- implies a @SqlQuery@ instance.
class Monad m => SqlQuery (m :: * -> *) where
    -- | Run a parameterized query that returns data.  The tuple argument is typically provided by
    -- one of the Quasiquoters: 'Preql.sql' or 'Preql.select'
    query :: (ToSql p, FromSql r, KnownNat (Width r)) => (Query (Width r), p) -> m (Vector r)

    -- | Run a parameterized query that does not return data.
    query_ :: ToSql p => (Query 0, p) -> m ()

-- | Most larger applications will define an instance; this one is suitable to test out the library.
instance SQL (ReaderT Connection IO) where
    withConnection = (ask >>=)

instance {-# OVERLAPPABLE #-} (Monad m, SQL m) => SqlQuery m where
    query qp = withConnection (\conn -> queryOn conn qp)
    query_ q = withConnection (\conn -> queryOn_ conn q)

-- * Transactions

-- | Run the provided 'Transaction'.  If it fails with a 'QueryError', roll back.
runTransactionIO :: IsolationLevel -> Transaction a -> Connection -> IO (Either QueryError a)
runTransactionIO level (Transaction m) conn = do
    either throwIO pure =<< W.begin conn level
    e_a <- runReaderT (runExceptT m) conn
    void $ case e_a of
        Left _  -> W.rollback conn
        Right _ -> W.commit conn
    return e_a

instance SqlQuery Transaction where
    query (q, p) = Transaction (ExceptT (ReaderT (\conn -> W.query conn q p)))
    query_ (q, p) = Transaction (ExceptT (ReaderT (\conn -> W.query_ conn q p)))

-- Transformer instances
-- These are all the same, except for the @withConnection@ definitions.

instance SQL m => SQL (ExceptT e m) where
    withConnection f = ExceptT (withConnection (runExceptT . f))
    runTransaction' level t = lift $ runTransaction' level t
    queryOn conn qp = lift $ queryOn conn qp
    queryOn_ conn q = lift $ queryOn_ conn q

instance {-# OVERLAPPABLE #-} SQL m => SQL (ReaderT r m) where
    withConnection f = ReaderT (\r -> withConnection (\conn -> runReaderT (f conn) r))
    runTransaction' level t = lift $ runTransaction' level t
    queryOn conn qp = lift $ queryOn conn qp
    queryOn_ conn q = lift $ queryOn_ conn q

instance SQL m => SQL (MaybeT m) where
    withConnection f = MaybeT (withConnection (runMaybeT . f))
    runTransaction' level t = lift $ runTransaction' level t
    queryOn conn qp = lift $ queryOn conn qp
    queryOn_ conn q = lift $ queryOn_ conn q

instance SQL m => SQL (SL.StateT s m) where
    withConnection f = SL.StateT (\s -> withConnection (\conn -> SL.runStateT (f conn) s))
    runTransaction' level t = lift $ runTransaction' level t
    queryOn conn qp = lift $ queryOn conn qp
    queryOn_ conn q = lift $ queryOn_ conn q

instance SQL m => SQL (SS.StateT s m) where
    withConnection f = SS.StateT (\s -> withConnection (\conn -> SS.runStateT (f conn) s))
    runTransaction' level t = lift $ runTransaction' level t
    queryOn conn qp = lift $ queryOn conn qp
    queryOn_ conn q = lift $ queryOn_ conn q

instance (Monoid w, SQL m) => SQL (S.RWST r w s m) where
    withConnection f = S.RWST (\r s  -> withConnection (\conn -> S.runRWST (f conn) r s))
    runTransaction' level t = lift $ runTransaction' level t
    queryOn conn qp = lift $ queryOn conn qp
    queryOn_ conn q = lift $ queryOn_ conn q

instance (Monoid w, SQL m) => SQL (L.RWST r w s m) where
    withConnection f = L.RWST (\r s  -> withConnection (\conn -> L.runRWST (f conn) r s))
    runTransaction' level t = lift $ runTransaction' level t
    queryOn conn qp = lift $ queryOn conn qp
    queryOn_ conn q = lift $ queryOn_ conn q
