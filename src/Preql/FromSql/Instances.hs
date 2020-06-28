{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Preql.FromSql.Instances where

import Preql.FromSql.Class
import Preql.FromSql.TH
import Preql.Wire.Errors
import Preql.Wire.Internal (applyDecoder)
import Preql.Wire.Types

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
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQL.Binary.Decoding as PGB
import qualified Preql.Wire.TypeInfo.Static as OID

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
