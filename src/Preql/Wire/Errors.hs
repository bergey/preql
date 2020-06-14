{-# LANGUAGE TemplateHaskell #-}
-- | Errors raised by functions in Preql.Wire

module Preql.Wire.Errors where

import Preql.Imports
import Preql.Wire.Orphans ()

import Data.Aeson.TH (defaultOptions, deriveJSON)

import qualified Database.PostgreSQL.LibPQ as PQ

-- | Errors that can occur in decoding a single field.
data UnlocatedFieldError
    = UnexpectedNull
    | ParseFailure Text
    deriving (Eq, Show, Typeable)
$(deriveJSON defaultOptions ''UnlocatedFieldError)

-- | A decoding error with information about the row & column of the result where it
-- occured.
data FieldError = FieldError
    { errorRow :: Int
    , errorColumn :: Int
    , failure :: UnlocatedFieldError
    } deriving (Eq, Show, Typeable)
instance Exception FieldError
$(deriveJSON defaultOptions ''FieldError)

data PgType = Oid PQ.Oid -- ^ A Postgres type with a known ID
    | TypeName Text -- ^ A Postgres type which we will need to lookup by name
    deriving (Eq, Show, Typeable)
$(deriveJSON defaultOptions ''PgType)

data TypeMismatch = TypeMismatch
    { expected :: PgType
    , actual :: PQ.Oid
    , column :: Int
    , columnName :: Maybe Text
    } deriving (Eq, Show, Typeable)
$(deriveJSON defaultOptions ''TypeMismatch)

data QueryError
    = ConnectionError Text
    | DecoderError FieldError
    | PgTypeMismatch [TypeMismatch]
    deriving (Eq, Show, Typeable)
instance Exception QueryError
$(deriveJSON defaultOptions ''QueryError)
