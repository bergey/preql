{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypedQuery where

import Data.Int (Int64)
import Database.PostgreSQL.Simple

-- | An SQL query, annotated with the types of its parameters and results.
newtype TypedQuery p r = TypedQuery Query
    deriving Show

runQuery :: (ToRow p, FromRow r) => Connection -> TypedQuery p r -> p -> IO [r]
runQuery conn (TypedQuery q) p = query conn q p

executeSql :: ToRow p => Connection -> TypedQuery p () -> p -> IO Int64
executeSql conn (TypedQuery q) p = execute conn q p
