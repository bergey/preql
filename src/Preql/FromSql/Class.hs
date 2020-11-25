{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies          #-}

module Preql.FromSql.Class where

import Preql.Wire.Errors
import Preql.Wire.Internal

import Control.Monad.Except
import Control.Monad.Trans.State
import GHC.TypeNats
import qualified BinaryParser as BP
import qualified Data.Vector.Sized as VS
import qualified Database.PostgreSQL.LibPQ as PQ

-- | A @FieldDecoder@ for a type @a@ consists of an OID indicating the
-- Postgres type which can be decoded, and a parser from the binary
-- representation of that type to the Haskell representation.
data FieldDecoder a = FieldDecoder PgType (BP.BinaryParser a)
    deriving Functor

class FromSqlField a where
    fromSqlField :: FieldDecoder a

-- | A type which can be decoded from a SQL row.  Note that this
-- includes the canonical order of fields.
--
-- The default (empty) instance works for any type with a
-- 'FromSqlField' instance
class FromSql a where
    -- | The number of columns read in decoding this type.
    type Width a :: Nat
    type Width a = 1
    fromSql :: RowDecoder (Width a) a
    default fromSql :: (FromSqlField a, Width a ~ 1) => RowDecoder (Width a) a
    fromSql = notNull fromSqlField

-- | Construct a decoder for a single non-nullable column.
notNull :: FieldDecoder a -> RowDecoder 1 a
notNull (FieldDecoder oid parser) = RowDecoder (VS.singleton oid) $ do
    m_bs <- getNextValue
    case m_bs of
        Nothing -> throwLocated UnexpectedNull
        Just bs -> either (throwLocated . ParseFailure) pure (BP.run parser bs)

-- | Construct a decoder for a single nullable column.
nullable :: FieldDecoder a -> RowDecoder 1 (Maybe a)
nullable (FieldDecoder oid parser) = RowDecoder (VS.singleton oid) $ do
    m_bs <- getNextValue
    case m_bs of
        Nothing -> return Nothing
        Just bs -> either (throwLocated . ParseFailure) (pure . Just) (BP.run parser bs)

throwLocated :: UnlocatedFieldError -> InternalDecoder a
throwLocated fieldError = do
    DecoderState{row = PQ.Row r, column = PQ.Col c} <- get
    throwError (FieldError (fromIntegral r) (fromIntegral c) fieldError)
