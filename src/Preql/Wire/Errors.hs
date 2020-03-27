-- | Errors raised by functions in Preql.Wire

module Preql.Wire.Errors where

import            Preql.Imports

import qualified Database.PostgreSQL.LibPQ as PQ

-- | An error with information about the row & column of the result where it occured.
data LocatedError a = LocatedError
    { errorRow :: PQ.Row
    , errorColumn :: PQ.Column
    , failure :: a
    } deriving (Eq, Show, Typeable)
instance (Show a, Typeable a) => Exception (LocatedError a)

-- | Errors that can occur in decoding a single field.
data FieldError
    = UnexpectedNull
    | ParseFailure Text
    deriving (Eq, Show, Typeable)

data DecoderError = FieldError (LocatedError FieldError) | PgTypeMismatch [TypeMismatch]
    deriving (Show, Eq, Typeable)
instance Exception DecoderError

data TypeMismatch = TypeMismatch
    { expected :: PQ.Oid
    , actual :: PQ.Oid
    , column :: PQ.Column
    , columnName :: Maybe Text
    } deriving (Eq, Show, Typeable)

data QueryError = QueryError Text | DecoderError DecoderError
    deriving (Eq, Show, Typeable)
instance Exception QueryError
