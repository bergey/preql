{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Preql.Wire.ToSql where

import Preql.Imports
import Preql.Wire.Tuples (deriveToSqlTuple)
import Preql.Wire.Types

import Data.Functor.Contravariant
import Data.Int
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.UUID (UUID)

import qualified ByteString.StrictBuilder as B
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQL.Binary.Encoding as PGB
import qualified Preql.Wire.TypeInfo.Static as OID

-- | A @FieldEncoder@ for a type @a@ consists of a function from @a@ to
-- it's binary representation, and an Postgres OID which tells
-- Postgres it's type & how to decode it.
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

-- | Types which can be encoded to a single Postgres field.
class ToSqlField a where
    toSqlField :: FieldEncoder a

-- | @ToSql a@ is sufficient to pass @a@ as parameters to a paramaterized query.
class ToSql a where
    toSql :: RowEncoder a

instance ToSqlField Bool where
    toSqlField = FieldEncoder OID.boolOid PGB.bool
instance ToSql Bool where toSql = oneField toSqlField

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

instance ToSqlField UUID where
    toSqlField = FieldEncoder OID.uuidOid PGB.uuid
instance ToSql UUID where toSql = oneField toSqlField

-- | If you want to encode some more specific Haskell type via JSON,
-- it is more efficient to use 'toSqlJsonField' rather than this
-- instance.
instance ToSqlField JSON.Value where
    toSqlField = FieldEncoder OID.jsonbOid PGB.jsonb_ast
instance ToSql JSON.Value where toSql = oneField toSqlField

toSqlJsonField :: JSON.ToJSON a => FieldEncoder a
toSqlJsonField = FieldEncoder OID.jsonbOid (PGB.jsonb_bytes . BSL.toStrict . JSON.encode)

instance ToSql () where
    toSql () = []

instance (ToSqlField a, ToSqlField b) => ToSql (a, b) where
    toSql (a, b) = [runFieldEncoder toSqlField a, runFieldEncoder toSqlField b]

instance (ToSqlField a, ToSqlField b, ToSqlField c) => ToSql (a, b, c) where
    toSql (a, b, c) =
        [runFieldEncoder toSqlField a, runFieldEncoder toSqlField b, runFieldEncoder toSqlField c]

-- The instances below all follow the pattern laid out by the tuple
-- instances above.  The ones above are written out without the macro
-- to illustrate the pattern.

$(deriveToSqlTuple 4)
$(deriveToSqlTuple 5)
$(deriveToSqlTuple 6)
$(deriveToSqlTuple 7)
$(deriveToSqlTuple 8)
$(deriveToSqlTuple 9)
$(deriveToSqlTuple 10)
$(deriveToSqlTuple 11)
$(deriveToSqlTuple 12)
$(deriveToSqlTuple 13)
$(deriveToSqlTuple 14)
$(deriveToSqlTuple 15)
$(deriveToSqlTuple 16)
$(deriveToSqlTuple 17)
$(deriveToSqlTuple 18)
$(deriveToSqlTuple 19)
$(deriveToSqlTuple 20)
$(deriveToSqlTuple 21)
$(deriveToSqlTuple 22)
$(deriveToSqlTuple 23)
$(deriveToSqlTuple 24)
$(deriveToSqlTuple 25)
