{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies          #-}

module Preql.FromSql.Class where

import Preql.Wire.Errors
import Preql.Wire.Internal

import Control.Exception (throwIO)
import Control.Monad.Except
import Control.Monad.Trans.Reader (ask)
import Data.IORef (readIORef)
import GHC.TypeNats
import qualified BinaryParser as BP
import qualified Data.Vector.Sized as VS
import qualified Database.PostgreSQL.LibPQ as PQ

-- | A @FieldDecoder@ for a type @a@ consists of an OID indicating the
-- Postgres type which can be decoded, and a parser from the binary
-- representation of that type to the Haskell representation.
data FieldDecoder a = FieldDecoder PgType (BP.BinaryParser a)
  deriving Functor

fieldParser :: FieldDecoder a -> BP.BinaryParser a
fieldParser (FieldDecoder _ parser) = parser

-- | A type which can be decoded from a single SQL field.  This is
-- mostly useful for defining what can be an element of an array or
-- 'Tuple'.
class FromSqlField a where
    fromSqlField :: FieldDecoder a

-- | A type which can be decoded from a SQL row.  Note that this
-- includes the canonical order of fields.
--
-- The default (empty) instance works for any type with a
-- 'FromSqlField' instance.  This is convenient when you define your
-- own Postgres types, since they should be instances of both type classes.
class FromSql a where
    -- | The number of columns read in decoding this type.
    type Width a :: Nat
    type Width a = 1
    {-# INLINE fromSql #-}
    fromSql :: RowDecoder (Width a) a
    default fromSql :: (FromSqlField a, Width a ~ 1) => RowDecoder (Width a) a
    fromSql = notNull fromSqlField

-- | Construct a decoder for a single non-nullable column.
{-# INLINE notNull #-}
notNull :: FieldDecoder a -> RowDecoder 1 a
notNull (FieldDecoder oid parser) = {-# SCC "notNull" #-} RowDecoder (VS.singleton oid) $ do
    m_bs <- getNextValue
    case m_bs of
        Nothing -> throwLocated UnexpectedNull
        Just bs -> either (throwLocated . ParseFailure) pure (BP.run parser bs)

-- | Construct a decoder for a single nullable column.
{-# INLINE nullable #-}
nullable :: FieldDecoder a -> RowDecoder 1 (Maybe a)
nullable (FieldDecoder oid parser) = {-# SCC "nullable" #-} RowDecoder (VS.singleton oid) $ do
    m_bs <- getNextValue
    case m_bs of
        Nothing -> return Nothing
        Just bs -> either (throwLocated . ParseFailure) (pure . Just) (BP.run parser bs)

{-# INLINE throwLocated #-}
throwLocated :: UnlocatedFieldError -> InternalDecoder a
throwLocated fieldError = {-# SCC "throwLocated" #-} do
    DecoderState{row = PQ.Row r, column = PQ.Col c} <- lift . readIORef =<< ask
    lift $ throwIO (FieldError (fromIntegral r) (fromIntegral c) fieldError)
