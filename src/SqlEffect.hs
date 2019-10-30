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
    runQuery :: (ToRow p, FromRow r) => TypedQuery p r -> p -> m [r]
    runQuery_ :: FromRow r => TypedQuery () r -> m [r]
    executeSql :: ToRow p => TypedQuery p () -> p -> m Int64

    default runQuery :: (MonadTrans t, SQL m', m ~ t m', ToRow p, FromRow r) => TypedQuery p r -> p -> m [r]
    runQuery q p = lift (runQuery q p)

    default runQuery_ :: (MonadTrans t, SQL m', m ~ t m', FromRow r) => TypedQuery () r -> m [r]
    runQuery_ = lift . runQuery_

    default executeSql :: (MonadTrans t, SQL m', m ~ t m', ToRow p) => TypedQuery p () -> p -> m Int64
    executeSql q p = lift (executeSql q p)

-- | Most larger applications will define an instance; this one is suitable to test out the library.
instance SQL (ReaderT Connection IO) where
    runQuery (TypedQuery q) p = do
        conn <- ask
        lift $ query conn q p
    runQuery_ (TypedQuery q) = do
        conn <- ask
        lift $ query_ conn q
    executeSql (TypedQuery q) p = do
        conn <- ask
        lift $ execute conn q p

instance SQL m => SQL (ExceptT e m)
instance SQL m => SQL (MaybeT m)
instance SQL m => SQL (StateT s m)
instance (Monoid w, SQL m) => SQL (WriterT w m)
instance {-# OVERLAPPABLE #-} SQL m => SQL (ReaderT r m)
