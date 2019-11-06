{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
-- | SQL Effect class, basically capturing some way of accessing a database.

module SqlEffect
    ( TypedQuery, SQL(..)
    ) where

import TypedQuery (TypedQuery(..))

import Database.PostgreSQL.Simple
import Data.Int (Int64)

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import Control.Monad.Trans.Class (MonadTrans(..))

import qualified TypedQuery as TQ

class Monad m => SQL (m :: * -> *) where
    query :: (ToRow p, FromRow r) => TypedQuery p r -> p -> m [r]
    query_ :: FromRow r => TypedQuery () r -> m [r]
    execute :: ToRow p => TypedQuery p () -> p -> m Int64

    default query :: (MonadTrans t, SQL m', m ~ t m', ToRow p, FromRow r) => TypedQuery p r -> p -> m [r]
    query q p = lift (runQuery q p)

    default query_ :: (MonadTrans t, SQL m', m ~ t m', FromRow r) => TypedQuery () r -> m [r]
    query_ = lift . runQuery_

    default execute :: (MonadTrans t, SQL m', m ~ t m', ToRow p) => TypedQuery p () -> p -> m Int64
    execute q p = lift (executeSql q p)

-- | Most larger applications will define an instance; this one is suitable to test out the library.
instance SQL (ReaderT Connection IO) where
    query q p = do
        conn <- ask
        lift $ TQ.query conn q p
    query_ q = do
        conn <- ask
        lift $ TQ.query_ conn q
    execute q p = do
        conn <- ask
        lift $ execute conn q p

instance SQL m => SQL (ExceptT e m)
instance SQL m => SQL (MaybeT m)
instance SQL m => SQL (StateT s m)
instance (Monoid w, SQL m) => SQL (WriterT w m)
instance {-# OVERLAPPABLE #-} SQL m => SQL (ReaderT r m)
