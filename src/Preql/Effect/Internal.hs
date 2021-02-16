{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | We use IO in the representation of Transaction, but we don't want to allow arbitrary IO, only
-- SQL queries.  So keep it private.

module Preql.Effect.Internal where

import Preql.Wire

import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Reader (ReaderT(..))

-- | A Transaction can only contain SQL queries (and pure functions).
newtype Transaction a = Transaction (ExceptT QueryError (ReaderT Connection IO) a)
    deriving newtype (Functor, Applicative, Monad)
