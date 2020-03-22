-- | Errors raised by functions in Preql.Wire

module Preql.Wire.Errors where

import            Preql.Imports

import qualified Database.PostgreSQL.LibPQ as PQ

data LocatedError a = LocatedError
    { errorRow :: PQ.Row
    , errorColumn :: PQ.Column
    , failure :: a
    } deriving (Eq, Show, Typeable)
instance (Show a, Typeable a) => Exception (LocatedError a)

data FieldError
    = UnexpectedNull
    | ParseFailure Text
    deriving (Eq, Show, Typeable)
