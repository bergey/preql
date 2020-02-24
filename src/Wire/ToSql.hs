{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.ToSql where

import           Imports

import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible
import           Data.Int

import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Builder as B
-- import qualified Data.ByteString.Lazy as BSL
import qualified ByteString.StrictBuilder as B
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

instance ToSqlField Text where
    toSqlField = FieldEncoder OID.textOid PGB.text_strict
instance ToSql Text where toSql = oneField toSqlField

instance ToSqlField ByteString where
    toSqlField = FieldEncoder OID.byteaOid PGB.bytea_strict
instance ToSql ByteString where toSql = oneField toSqlField

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
