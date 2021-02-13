{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Preql.FromSql.Instances where

import Preql.FromSql.Tuple
import           Preql.FromSql.Class
import           Preql.FromSql.TH
import           Preql.Wire.Errors
import           Preql.Wire.Internal        (applyDecoder)
import           Preql.Wire.Types

import qualified BinaryParser               as BP
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Int
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Time                  (Day, TimeOfDay, UTCTime)
import           Data.UUID                  (UUID)
import qualified Database.PostgreSQL.LibPQ  as PQ
import           GHC.TypeNats
import qualified PostgreSQL.Binary.Decoding as PGB
import           Preql.Imports
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

instance FromSqlField Char where
    fromSqlField = FieldDecoder (Oid OID.charOid) PGB.char
instance FromSql Char where fromSql = notNull fromSqlField

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

instance FromSqlField PgName where
    fromSqlField = FieldDecoder (Oid OID.nameOid) (PgName <$> PGB.text_strict)
instance FromSql PgName where fromSql = notNull fromSqlField

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

instance (FromSqlField a, FromSqlField b) => FromSqlField (Tuple (a, b)) where
  fromSqlField =
    let vc = PGB.valueComposite . fieldParser
    in FieldDecoder (Oid OID.recordOid) (Tuple <$> PGB.composite (pure (,) <*> vc fromSqlField <*> vc fromSqlField))
instance (FromSqlField a, FromSqlField b) => FromSql (Tuple (a, b))

$(deriveFromSqlFieldTuple 3)
$(deriveFromSqlFieldTuple 4)
$(deriveFromSqlFieldTuple 5)
$(deriveFromSqlFieldTuple 6)
$(deriveFromSqlFieldTuple 7)
$(deriveFromSqlFieldTuple 8)
$(deriveFromSqlFieldTuple 9)
$(deriveFromSqlFieldTuple 10)
$(deriveFromSqlFieldTuple 11)
$(deriveFromSqlFieldTuple 12)
$(deriveFromSqlFieldTuple 13)
$(deriveFromSqlFieldTuple 14)
$(deriveFromSqlFieldTuple 15)
$(deriveFromSqlFieldTuple 16)
$(deriveFromSqlFieldTuple 17)
$(deriveFromSqlFieldTuple 18)
$(deriveFromSqlFieldTuple 19)
$(deriveFromSqlFieldTuple 20)
$(deriveFromSqlFieldTuple 21)
$(deriveFromSqlFieldTuple 22)
$(deriveFromSqlFieldTuple 23)
$(deriveFromSqlFieldTuple 24)
$(deriveFromSqlFieldTuple 25)
