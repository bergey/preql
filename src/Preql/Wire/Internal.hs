{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

-- | The types in this module have invariants which cannot be checked
-- if their constructors are in scope.  Preql.Wire exports the type
-- names only.

module Preql.Wire.Internal where

import Preql.Wire.Errors

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.String (IsString)
import GHC.TypeNats
import Preql.Imports

import qualified Data.Vector.Sized as VS
import qualified Database.PostgreSQL.LibPQ as PQ

-- | The IsString instance does no validation; the limited instances
-- discourage directly manipulating strings, with the high risk of SQL
-- injection.  A @Query@ is tagged with a 'Nat' representing the width
-- of its return type.
newtype Query (n :: Nat) = Query ByteString
    deriving (Show, IsString)

-- | @RowDecoder@ is 'Functor' but not 'Monad' so that we can index
-- the type by the number of columns that it consumes.  We also know &
-- verify all of the OIDs before we read any of the field data sent by
-- Postgres, which would admit an 'Applicative' instance but not 'Monad'
data RowDecoder (n :: Nat) a = RowDecoder (VS.Vector n PgType) (InternalDecoder a)
    deriving Functor

-- | Analogous to 'pure', @pureDecoder a@ returns the value @a@
-- without consuming any input from Postgres.
pureDecoder :: a -> RowDecoder 0 a
pureDecoder a = RowDecoder VS.empty (pure a)

-- | Analogous to '<*>', @pureDecoder Constructor `applyDecoder` a
-- `applyDecoder` b@ supplies two arguments to @Constructor@, from the
-- 'RowDecoder' @a@ and @b@.
applyDecoder :: RowDecoder m (a -> b) -> RowDecoder n a -> RowDecoder (m+n) b
applyDecoder (RowDecoder vm f) (RowDecoder vn a) = RowDecoder (vm VS.++ vn) (f <*> a)

-- TODO can I use ValidationT instead of ExceptT, since I ensure Column is incremented before errors?
-- | Internal because we need IO for the libpq FFI, but we promise not
-- to do any IO besides decoding.  We don't even make network calls to
-- Postgres in @InternalDecoder@
type InternalDecoder =  StateT DecoderState (ExceptT FieldError IO)

data DecoderState = DecoderState
    { result :: PQ.Result
    , row    :: PQ.Row
    , column :: PQ.Column
    }
    deriving (Show, Eq)

decodeRow :: RowDecoder n a -> PQ.Result -> PQ.Row -> ExceptT FieldError IO a
decodeRow (RowDecoder _ parsers) result row =
    evalStateT parsers (DecoderState result row 0)

getNextValue :: InternalDecoder (Maybe ByteString)
getNextValue = do
    s@DecoderState{..} <- get
    put (s { column = column + 1 } :: DecoderState)
    liftIO $ PQ.getvalue result row column
