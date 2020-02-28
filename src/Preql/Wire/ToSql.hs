{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Preql.Wire.ToSql where

import           Preql.Imports
import           Preql.Wire.Types

import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible
import           Data.Int
import           Data.Time (Day, TimeOfDay, UTCTime, TimeZone)

import qualified ByteString.StrictBuilder as B
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as OID
import qualified PostgreSQL.Binary.Encoding as PGB

data FieldEncoder a = FieldEncoder PQ.Oid (a -> B.Builder)

instance Contravariant FieldEncoder where
    contramap f (FieldEncoder oid enc) = FieldEncoder oid (enc . f)

runFieldEncoder :: FieldEncoder p -> p -> (PQ.Oid, ByteString)
runFieldEncoder (FieldEncoder oid enc) p = (oid, B.builderBytes (enc p))

type RowEncoder a = a -> [(PQ.Oid, ByteString)]

runEncoder :: RowEncoder p -> p -> [Maybe (PQ.Oid, ByteString, PQ.Format)]
runEncoder fields p = fields p <&> \(oid, bs) -> Just (oid, bs, PQ.Binary)

oneField :: FieldEncoder a -> RowEncoder a
oneField enc = \p -> [runFieldEncoder enc p]

class ToSqlField a where
    toSqlField :: FieldEncoder a

class ToSql a where
    toSql :: RowEncoder a

instance ToSqlField Int16 where
    toSqlField = FieldEncoder OID.int2Oid PGB.int2_int16
instance ToSql Int16 where toSql = oneField toSqlField

instance ToSqlField Int32 where
    toSqlField = FieldEncoder OID.int4Oid PGB.int4_int32
instance ToSql Int32 where toSql = oneField toSqlField

instance ToSqlField Int64 where
    toSqlField = FieldEncoder OID.int8Oid PGB.int8_int64
instance ToSql Int64 where toSql = oneField toSqlField

instance ToSqlField Float where
    toSqlField = FieldEncoder OID.float4Oid PGB.float4
instance ToSql Float where toSql = oneField toSqlField

instance ToSqlField Double where
    toSqlField = FieldEncoder OID.float8Oid PGB.float8
instance ToSql Double where toSql = oneField toSqlField

instance ToSqlField Char where
    toSqlField = FieldEncoder OID.charOid PGB.char_utf8
instance ToSql Char where toSql = oneField toSqlField

instance ToSqlField String where
    toSqlField = FieldEncoder OID.textOid (PGB.text_strict . T.pack)
instance ToSql String where toSql = oneField toSqlField

instance ToSqlField Text where
    toSqlField = FieldEncoder OID.textOid PGB.text_strict
instance ToSql Text where toSql = oneField toSqlField

instance ToSqlField TL.Text where
    toSqlField = FieldEncoder OID.textOid PGB.text_lazy
instance ToSql TL.Text where toSql = oneField toSqlField

-- | If you want to encode some more specific Haskell type via JSON,
-- it is more efficient to use 'Data.Aeson.encode' and
-- 'PostgreSQL.Binary.Encoding.jsonb_bytes' directly, rather than this
-- instance.
instance ToSqlField ByteString where
    toSqlField = FieldEncoder OID.byteaOid PGB.bytea_strict
instance ToSql ByteString where toSql = oneField toSqlField

instance ToSqlField BSL.ByteString where
    toSqlField = FieldEncoder OID.byteaOid PGB.bytea_lazy
instance ToSql BSL.ByteString where toSql = oneField toSqlField

-- TODO check for integer_datetimes setting
instance ToSqlField UTCTime where
    toSqlField = FieldEncoder OID.timestamptzOid PGB.timestamptz_int
instance ToSql UTCTime where toSql = oneField toSqlField

instance ToSqlField Day where
    toSqlField = FieldEncoder OID.dateOid PGB.date
instance ToSql Day where toSql = oneField toSqlField

instance ToSqlField TimeOfDay where
    toSqlField = FieldEncoder OID.timeOid PGB.time_int
instance ToSql TimeOfDay where toSql = oneField toSqlField

instance ToSqlField TimeTZ where
    toSqlField = FieldEncoder OID.timetzOid (\(TimeTZ tod tz) -> PGB.timetz_int (tod, tz))
instance ToSql TimeTZ where toSql = oneField toSqlField

-- | If you want to encode some more specific Haskell type via JSON,
-- it is more efficient to use 'Data.Aeson.encode' and
-- 'PostgreSQL.Binary.Encoding.jsonb_bytes' directly, rather than this
-- instance.
instance ToSqlField JSON.Value where
    toSqlField = FieldEncoder OID.jsonbOid PGB.jsonb_ast
instance ToSql JSON.Value where toSql = oneField toSqlField

instance ToSql () where
    toSql () = []

instance (ToSqlField a, ToSqlField b) => ToSql (a, b) where
    toSql (a, b) = [runFieldEncoder toSqlField a, runFieldEncoder toSqlField b]

instance (ToSqlField a, ToSqlField b, ToSqlField c) => ToSql (a, b, c) where
    toSql (a, b, c) =
        [runFieldEncoder toSqlField a, runFieldEncoder toSqlField b, runFieldEncoder toSqlField c]

instance (ToSqlField a, ToSqlField b, ToSqlField c, ToSqlField d) => ToSql (a, b, c, d) where
    toSql (a, b, c, d) =
        [runFieldEncoder toSqlField a, runFieldEncoder toSqlField b, runFieldEncoder toSqlField c
        , runFieldEncoder toSqlField d]

instance (ToSqlField a, ToSqlField b, ToSqlField c, ToSqlField d, ToSqlField e) =>
    ToSql (a, b, c, d, e) where
    toSql (a, b, c, d, e) =
        [runFieldEncoder toSqlField a, runFieldEncoder toSqlField b, runFieldEncoder toSqlField c
        , runFieldEncoder toSqlField d, runFieldEncoder toSqlField e]
