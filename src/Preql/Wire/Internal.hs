{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The types in this module have invariants which cannot be checked
-- if their constructors are in scope.  Preql.Wire exports the type
-- names only.

module Preql.Wire.Internal where

import Preql.Wire.Errors

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.String (IsString)
import Preql.Imports

import qualified Database.PostgreSQL.LibPQ as PQ

-- TODO less ambiguous name (or rename others)
-- | The IsString instance does no validation; the limited instances
-- discourage directly manipulating strings, with the high risk of SQL
-- injection.
newtype Query = Query ByteString
    deriving (Show, IsString)

-- TODO PgType for non-builtin types
-- | @RowDecoder@ is 'Applicative' but not 'Monad' so that we can
-- assemble all of the OIDs before we read any of the field data sent
-- by Postgresj.
data RowDecoder a = RowDecoder [PQ.Oid] (InternalDecoder a)
    deriving Functor

instance Applicative RowDecoder where
    pure a = RowDecoder [] (pure a)
    RowDecoder t1 p1 <*> RowDecoder t2 p2 = RowDecoder (t1 <> t2) (p1 <*> p2)

-- TODO can I use ValidationT instead of ExceptT, since I ensure Column is incremented before errors?
type InternalDecoder =  StateT DecoderState (ExceptT FieldError IO)

data DecoderState = DecoderState
    { result :: PQ.Result
    , row :: PQ.Row
    , column :: PQ.Column
    } deriving (Show, Eq)

decodeRow :: RowDecoder a -> PQ.Result -> PQ.Row -> ExceptT FieldError IO a
decodeRow (RowDecoder _ parsers) result row =
    evalStateT parsers (DecoderState result row 0)

getNextValue :: InternalDecoder (Maybe ByteString)
getNextValue = do
    s@DecoderState{..} <- get
    put (s { column = column + 1 } :: DecoderState)
    liftIO $ PQ.getvalue result row column
