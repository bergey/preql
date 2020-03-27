-- | Errors raised by functions in Preql.Wire

module Preql.Wire.Errors where

import            Preql.Imports

import qualified Database.PostgreSQL.LibPQ as PQ

-- | An error with information about the row & column of the result where it occured.
data FieldError = FieldError
    { errorRow :: PQ.Row
    , errorColumn :: PQ.Column
    , failure :: UnlocatedFieldError
    } deriving (Eq, Show, Typeable)
instance Exception FieldError

-- | Errors that can occur in decoding a single field.
data UnlocatedFieldError
    = UnexpectedNull
    | ParseFailure Text
    deriving (Eq, Show, Typeable)

data TypeMismatch = TypeMismatch
    { expected :: PQ.Oid
    , actual :: PQ.Oid
    , column :: PQ.Column
    , columnName :: Maybe Text
    } deriving (Eq, Show, Typeable)

data QueryError
    = ConnectionError Text
    | DecoderError FieldError
    | PgTypeMismatch [TypeMismatch]
    deriving (Eq, Show, Typeable)
instance Exception QueryError
