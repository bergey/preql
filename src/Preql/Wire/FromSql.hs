{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor     #-}

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
import Preql.Imports

import qualified BinaryParser as BP
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQL.Binary.Decoding as PGB
import qualified Preql.Wire.TypeInfo.Static as OID

-- | A @FieldDecoder@ for a type @a@ consists of an OID indicating the
-- Postgres type which can be decoded, and a parser from the binary
-- representation of that type to the Haskell representation.
data FieldDecoder a = FieldDecoder PQ.Oid (BP.BinaryParser a)
    deriving Functor

throwLocated :: UnlocatedFieldError -> InternalDecoder a
throwLocated failure = do
    DecoderState{row = PQ.Row r, column = PQ.Col c} <- get
    throwError (FieldError (fromIntegral r) (fromIntegral c) failure)

decodeVector :: RowDecoder a -> PQ.Result -> IO (Either QueryError (Vector a))
decodeVector rd@(RowDecoder oids _parsers) result = do
    mismatches <- fmap catMaybes $ for (zip [0 ..] oids) $ \(column@(PQ.Col cint), expected) -> do
        actual <- liftIO $ PQ.ftype result column
        if actual == expected
            then return Nothing
            else do
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

notNull :: FieldDecoder a -> RowDecoder a
notNull (FieldDecoder oid parser) = RowDecoder [oid] $ do
    m_bs <- getNextValue
    case m_bs of
        Nothing -> throwLocated UnexpectedNull
        Just bs -> either (throwLocated . ParseFailure) pure (BP.run parser bs)

nullable :: FieldDecoder a -> RowDecoder (Maybe a)
nullable (FieldDecoder oid parser) = RowDecoder [oid] $ do
    m_bs <- getNextValue
    case m_bs of
        Nothing -> return Nothing
        Just bs -> either (throwLocated . ParseFailure) (pure . Just) (BP.run parser bs)

class FromSqlField a where
    fromSqlField :: FieldDecoder a

class FromSql a where
    fromSql :: RowDecoder a

instance FromSqlField Bool where
    fromSqlField = FieldDecoder OID.boolOid PGB.bool
instance FromSql Bool where fromSql = notNull fromSqlField

instance FromSqlField Int16 where
    fromSqlField = FieldDecoder OID.int2Oid PGB.int
instance FromSql Int16 where fromSql = notNull fromSqlField

instance FromSqlField Int32 where
    fromSqlField = FieldDecoder OID.int4Oid PGB.int
instance FromSql Int32 where fromSql = notNull fromSqlField

instance FromSqlField Int64  where
    fromSqlField = FieldDecoder OID.int8Oid PGB.int
instance FromSql Int64 where fromSql = notNull fromSqlField

instance FromSqlField Float where
    fromSqlField = FieldDecoder OID.float4Oid PGB.float4
instance FromSql Float where fromSql = notNull fromSqlField

instance FromSqlField Double where
    fromSqlField = FieldDecoder OID.float8Oid PGB.float8
instance FromSql Double where fromSql = notNull fromSqlField

-- TODO does Postgres have a single-char type?  Does it always return bpchar?
-- instance FromSqlField Char where
--     fromSqlField = FieldDecoder OID.charOid PGB.char
-- instance FromSql Char where fromSql = notNull fromSqlField

instance FromSqlField String where
    fromSqlField = FieldDecoder OID.textOid (T.unpack <$> PGB.text_strict)
instance FromSql String where fromSql = notNull fromSqlField

instance FromSqlField Text where
    fromSqlField = FieldDecoder OID.textOid PGB.text_strict
instance FromSql Text where fromSql = notNull fromSqlField

instance FromSqlField TL.Text where
    fromSqlField = FieldDecoder OID.textOid PGB.text_lazy
instance FromSql TL.Text where fromSql = notNull fromSqlField

-- | If you want to encode some more specific Haskell type via JSON,
-- it is more efficient to use 'Data.Aeson.encode' and
-- 'PostgreSQL.Binary.Encoding.jsonb_bytes' directly, rather than this
-- instance.
instance FromSqlField ByteString where
    fromSqlField = FieldDecoder OID.byteaOid (BS.copy <$> BP.remainders)
instance FromSql ByteString where fromSql = notNull fromSqlField

instance FromSqlField BSL.ByteString where
    fromSqlField = FieldDecoder OID.byteaOid (BSL.fromStrict . BS.copy <$> BP.remainders)
instance FromSql BSL.ByteString where fromSql = notNull fromSqlField

-- TODO check for integer_datetimes setting
instance FromSqlField UTCTime where
    fromSqlField = FieldDecoder OID.timestamptzOid PGB.timestamptz_int
instance FromSql UTCTime where fromSql = notNull fromSqlField

instance FromSqlField Day where
    fromSqlField = FieldDecoder OID.dateOid PGB.date
instance FromSql Day where fromSql = notNull fromSqlField

instance FromSqlField TimeOfDay where
    fromSqlField = FieldDecoder OID.timeOid PGB.time_int
instance FromSql TimeOfDay where fromSql = notNull fromSqlField

instance FromSqlField TimeTZ where
    fromSqlField = FieldDecoder OID.timetzOid (uncurry TimeTZ <$> PGB.timetz_int)
instance FromSql TimeTZ where fromSql = notNull fromSqlField

instance FromSqlField UUID where
    fromSqlField = FieldDecoder OID.uuidOid PGB.uuid
instance FromSql UUID where fromSql = notNull fromSqlField

-- | If you want to encode some more specific Haskell type via JSON,
-- it is more efficient to use 'fromSqlJsonField' rather than this
-- instance.
instance FromSqlField JSON.Value where
    fromSqlField = FieldDecoder OID.jsonbOid PGB.jsonb_ast
instance FromSql JSON.Value where fromSql = notNull fromSqlField

fromSqlJsonField :: JSON.FromJSON a => FieldDecoder a
fromSqlJsonField = FieldDecoder OID.jsonbOid
    (PGB.jsonb_bytes (first T.pack . JSON.eitherDecode . BSL.fromStrict))

-- Overlappable so applications can write Maybe for multi-field domain types
instance {-# OVERLAPPABLE #-} FromSqlField a => FromSql (Maybe a) where
    fromSql = nullable fromSqlField

instance (FromSql a, FromSql b) => FromSql (a, b) where
    fromSql = (,) <$> fromSql <*> fromSql

instance (FromSql a, FromSql b, FromSql c) => FromSql (a, b, c) where
    fromSql = (,,) <$> fromSql <*> fromSql <*> fromSql

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
