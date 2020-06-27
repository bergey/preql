{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Decoding values from Postgres wire format to Haskell.

module Preql.Wire.FromSql where

import Preql.Wire.Errors
import Preql.Wire.Internal
import Preql.Wire.Tuples (deriveFromSqlTuple)
import Preql.Wire.Types

import Control.Monad.Except
import Control.Monad.Trans.State
import Data.Int
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.UUID (UUID)
import GHC.TypeNats
import Preql.Imports

import qualified BinaryParser as BP
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQL.Binary.Decoding as PGB
import qualified Preql.Wire.TypeInfo.Static as OID

-- | A @FieldDecoder@ for a type @a@ consists of an OID indicating the
-- Postgres type which can be decoded, and a parser from the binary
-- representation of that type to the Haskell representation.
data FieldDecoder a = FieldDecoder PgType (BP.BinaryParser a)
    deriving Functor

throwLocated :: UnlocatedFieldError -> InternalDecoder a
throwLocated failure = do
    DecoderState{row = PQ.Row r, column = PQ.Col c} <- get
    throwError (FieldError (fromIntegral r) (fromIntegral c) failure)

decodeVector :: KnownNat n =>
    (PgType -> IO (Either QueryError PQ.Oid)) -> RowDecoder n a -> PQ.Result -> IO (Either QueryError (Vector a))
decodeVector lookupType rd@(RowDecoder pgtypes _parsers) result = do
    mismatches <- fmap (catMaybes . VS.toList) $ for (VS.zip (VS.enumFromN 0) pgtypes) $ \(column@(PQ.Col cint), expected) -> do
        actual <- PQ.ftype result column
        e_expectedOid <- lookupType expected
        case e_expectedOid of
            Right oid | actual == oid -> return Nothing
            _ -> do
                m_name <- liftIO $ PQ.fname result column
                let columnName = decodeUtf8With lenientDecode <$> m_name
                return $ Just (TypeMismatch{column = fromIntegral cint, ..})
    if not (null mismatches)
        then return (Left (PgTypeMismatch mismatches))
        else do
            (PQ.Row ntuples) <- liftIO $ PQ.ntuples result
            let toRow = PQ.toRow . fromIntegral
            fmap (first DecoderError) . runExceptT $
                V.generateM (fromIntegral ntuples) (decodeRow rd result . toRow)

notNull :: FieldDecoder a -> RowDecoder 1 a
notNull (FieldDecoder oid parser) = RowDecoder (VS.singleton oid) $ do
    m_bs <- getNextValue
    case m_bs of
        Nothing -> throwLocated UnexpectedNull
        Just bs -> either (throwLocated . ParseFailure) pure (BP.run parser bs)

nullable :: FieldDecoder a -> RowDecoder 1 (Maybe a)
nullable (FieldDecoder oid parser) = RowDecoder (VS.singleton oid) $ do
    m_bs <- getNextValue
    case m_bs of
        Nothing -> return Nothing
        Just bs -> either (throwLocated . ParseFailure) (pure . Just) (BP.run parser bs)

class FromSqlField a where
    fromSqlField :: FieldDecoder a

-- | A type which can be decoded from a SQL row.  Note that this
-- includes the canonical order of fields.
--
-- The default (empty) instance works for any type with a
-- 'FromSqlField' instance
class FromSql a where
    type Width a :: Nat
    type Width a = 1

    fromSql :: RowDecoder (Width a) a
    default fromSql :: (FromSqlField a, Width a ~ 1) => RowDecoder (Width a) a
    fromSql = notNull fromSqlField

instance FromSqlField Bool where
    fromSqlField = FieldDecoder (Oid OID.boolOid) PGB.bool
instance FromSql Bool

instance FromSqlField Int16 where
    fromSqlField = FieldDecoder (Oid OID.int2Oid) PGB.int
instance FromSql Int16

instance FromSqlField Int32 where
    fromSqlField = FieldDecoder (Oid OID.int4Oid) PGB.int
instance FromSql Int32

instance FromSqlField Int64  where
    fromSqlField = FieldDecoder (Oid OID.int8Oid) PGB.int
instance FromSql Int64

instance FromSqlField Float where
    fromSqlField = FieldDecoder (Oid OID.float4Oid) PGB.float4
instance FromSql Float

instance FromSqlField Double where
    fromSqlField = FieldDecoder (Oid OID.float8Oid) PGB.float8
instance FromSql Double

-- TODO does Postgres have a single-char type?  Does it always return bpchar?
-- instance FromSqlField Char where
--     fromSqlField = FieldDecoder (Oid OID.charOid) PGB.char
-- instance FromSql Char

instance FromSqlField String where
    fromSqlField = FieldDecoder (Oid OID.textOid) (T.unpack <$> PGB.text_strict)
instance FromSql String

instance FromSqlField Text where
    fromSqlField = FieldDecoder (Oid OID.textOid) PGB.text_strict
instance FromSql Text

instance FromSqlField TL.Text where
    fromSqlField = FieldDecoder (Oid OID.textOid) PGB.text_lazy
instance FromSql TL.Text

-- | If you want to encode some more specific Haskell type via JSON,
-- it is more efficient to use 'Data.Aeson.encode' and
-- 'PostgreSQL.Binary.Encoding.jsonb_bytes' directly, rather than this
-- instance.
instance FromSqlField ByteString where
    fromSqlField = FieldDecoder (Oid OID.byteaOid) (BS.copy <$> BP.remainders)
instance FromSql ByteString

instance FromSqlField BSL.ByteString where
    fromSqlField = FieldDecoder (Oid OID.byteaOid) (BSL.fromStrict . BS.copy <$> BP.remainders)
instance FromSql BSL.ByteString

-- TODO check for integer_datetimes setting
instance FromSqlField UTCTime where
    fromSqlField = FieldDecoder (Oid OID.timestamptzOid) PGB.timestamptz_int
instance FromSql UTCTime

instance FromSqlField Day where
    fromSqlField = FieldDecoder (Oid OID.dateOid) PGB.date
instance FromSql Day

instance FromSqlField TimeOfDay where
    fromSqlField = FieldDecoder (Oid OID.timeOid) PGB.time_int
instance FromSql TimeOfDay

instance FromSqlField TimeTZ where
    fromSqlField = FieldDecoder (Oid OID.timetzOid) (uncurry TimeTZ <$> PGB.timetz_int)
instance FromSql TimeTZ

instance FromSqlField UUID where
    fromSqlField = FieldDecoder (Oid OID.uuidOid) PGB.uuid
instance FromSql UUID

instance FromSqlField PQ.Oid where
    fromSqlField = PQ.Oid <$> FieldDecoder (Oid OID.oidOid) PGB.int
instance FromSql PQ.Oid

-- | If you want to encode some more specific Haskell type via JSON,
-- it is more efficient to use 'fromSqlJsonField' rather than this
-- instance.
instance FromSqlField JSON.Value where
    fromSqlField = FieldDecoder (Oid OID.jsonbOid) PGB.jsonb_ast
instance FromSql JSON.Value

fromSqlJsonField :: JSON.FromJSON a => FieldDecoder a
fromSqlJsonField = FieldDecoder (Oid OID.jsonbOid)
    (PGB.jsonb_bytes (first T.pack . JSON.eitherDecode . BSL.fromStrict))

-- Overlappable so applications can write Maybe for multi-field domain types
instance {-# OVERLAPPABLE #-} FromSqlField a => FromSql (Maybe a) where
    fromSql = nullable fromSqlField

instance (FromSql a, FromSql b) => FromSql (a, b) where
    type Width (a, b) = Width a + Width b
    fromSql = ((,) <$> fromSql) `applyDecoder` fromSql

instance (FromSql a, FromSql b, FromSql c) => FromSql (a, b, c) where
    type Width (a, b, c) = (Width a + Width b) + Width c
    fromSql = ((,,) <$> fromSql) `applyDecoder` fromSql `applyDecoder` fromSql

-- The instances below all follow the pattern laid out by the tuple
-- instances above.  The ones above are written out without the macro
-- to illustrate the pattern.

$(deriveFromSqlTuple 4)
$(deriveFromSqlTuple 5)
$(deriveFromSqlTuple 6)
$(deriveFromSqlTuple 7)
$(deriveFromSqlTuple 8)
$(deriveFromSqlTuple 9)
$(deriveFromSqlTuple 10)
$(deriveFromSqlTuple 11)
$(deriveFromSqlTuple 12)
$(deriveFromSqlTuple 13)
$(deriveFromSqlTuple 14)
$(deriveFromSqlTuple 15)
$(deriveFromSqlTuple 16)
$(deriveFromSqlTuple 17)
$(deriveFromSqlTuple 18)
$(deriveFromSqlTuple 19)
$(deriveFromSqlTuple 20)
$(deriveFromSqlTuple 21)
$(deriveFromSqlTuple 22)
$(deriveFromSqlTuple 23)
$(deriveFromSqlTuple 24)
$(deriveFromSqlTuple 25)
