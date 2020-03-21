{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | We use IO in the representation of Transaction, but we don't want
-- to allow arbitrary IO, only SQL queries.  So keep it private.
--
-- The SQL class refers to Transaction, and the Transaction instance
-- refers to the class, so it all needs to be here.

module Preql.Effect.Internal where

import Preql.Imports
import Preql.Wire
import Preql.Wire.Internal

import Control.Exception (throwIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Database.PostgreSQL.LibPQ (Connection)

import qualified Preql.Wire.Query as W

-- | An Effect class for running SQL queries.  You can think of this
-- as a context specifying a particular Postgres connection (or connection
-- pool).
class Monad m => SQL (m :: * -> *) where
    query :: (ToSql p, FromSql r) => (Query, p) -> m (Vector r)
    query = runTransaction . query

    query_ :: ToSql p => (Query, p) -> m ()
    query_ = runTransaction . query_

    runTransaction :: Transaction a -> m a
    -- TODO add variant with isolation level

-- | Most larger applications will define an instance; this one is suitable to test out the library.
instance SQL (ReaderT Connection IO) where
    runTransaction t = do
        conn <- ask
        lift (either throwIO pure =<< runTransactionIO t conn)

instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad (t m), SQL m) => SQL (t m) where
    query = lift . query
    query_ = lift . query_
    runTransaction = lift . runTransaction

-- * Transactions

-- | A Transaction can only contain SQL queries (and pure functions).
newtype Transaction a = Transaction (ExceptT W.QueryError (ReaderT Connection IO) a)
    deriving newtype (Functor, Applicative, Monad)

runTransactionIO :: Transaction a -> Connection -> IO (Either W.QueryError a)
runTransactionIO (Transaction m) conn = do
    void $ W.query_ conn (Query "BEGIN TRANSACTION") ()
    e_a <- runReaderT (runExceptT m) conn
    void $ case e_a of
        Left _ -> W.query_ conn (Query "ROLLBACK") ()
        Right _ -> W.query_ conn (Query "COMMIT") ()
    return e_a

instance SQL Transaction where
    query (q, p) = Transaction (ExceptT (ReaderT (\conn -> W.query conn q p)))
    query_ (q, p) = Transaction (ExceptT (ReaderT (\conn -> W.query_ conn q p)))
    runTransaction (Transaction t) = do
        query_ (Query "SAVEPOINT preql_savepoint", ())
        Transaction . ExceptT . ReaderT $ \conn -> do
            e_a <- runReaderT (runExceptT t) conn
            case e_a of
                Right _ -> void $ W.query_ conn (Query "RELEASE SAVEPOINT preql_savepoint") ()
                Left _ -> do
                    void $ W.query_ conn (Query "ROLLBACK SAVEPOINT preql_savepoint") ()
                    -- release after rollback, so a later rolback catches the outer same-named savepoint
                    void $ W.query_ conn (Query "RELEASE SAVEPOINT preql_savepoint") ()
            return e_a
