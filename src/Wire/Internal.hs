{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module Wire.Internal where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.String (IsString)
import           Imports

import qualified Database.PostgreSQL.LibPQ as PQ

-- | A @Query@ is a string ready to be passed to Postgres, with
-- phantom type parameters describing its parameters and result.
-- Depending how the @Query@ was constructed, these parameters may be
-- inferred from context (offering no added type safety), or be
-- partly synthesized from the underlying string.
--
-- The IsString instance does no validation; the limited instances
-- discourage directly manipulating strings, with the high risk of SQL
-- injection.
newtype Query params result = Query ByteString
    deriving (Show, IsString)

-- TODO PgType for non-builtin types
data RowDecoder a = RowDecoder [PQ.Oid] (InternalDecoder a)
    deriving Functor

instance Applicative RowDecoder where
    pure a = RowDecoder [] (pure a)
    RowDecoder t1 p1 <*> RowDecoder t2 p2 = RowDecoder (t1 <> t2) (p1 <*> p2)

-- TODO can I use ValidationT instead of ExceptT, since I ensure Column is incremented before errors?
type InternalDecoder =  StateT DecoderState (ExceptT (LocatedError FieldError) IO)

data DecoderState = DecoderState
    { result :: PQ.Result
    , row :: PQ.Row
    , column :: PQ.Column
    } deriving (Show, Eq)

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

decodeRow :: RowDecoder a -> PQ.Result -> PQ.Row -> ExceptT (LocatedError FieldError) IO a
decodeRow (RowDecoder _ parsers) result row =
    evalStateT parsers (DecoderState result row 0)

getNextValue :: InternalDecoder (Maybe ByteString)
getNextValue = do
    s@DecoderState{..} <- get
    put (s { column = column + 1 } :: DecoderState)
    liftIO $ PQ.getvalue result row column
