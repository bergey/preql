{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Preql.FromSql.Instances (fromSqlJsonField) where

import Preql.FromSql.Class
import Preql.FromSql.TH
import Preql.FromSql.Tuple
import Preql.Wire.Errors
import Preql.Wire.Internal (applyDecoder)
import Preql.Wire.Types

import Data.Int
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.LibPQ (Oid)
import GHC.Exts (maxTupleSize)
import GHC.TypeNats
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

-- We write @FromSql@ instances for each primitive type, rather than a single
-- @FromSqlField a => FromSql a@ in order to give better type errors.  The
-- general instance above would overlap with every instance for a user-defined
-- type (for a table), and the error would suggest defining @FromSqlField@,
-- which is usually not the right answer.  New field types (like enums) are much
-- less common than new tables, so this seems like a good tradeoff.

instance FromSqlField Bool where
    fromSqlField = FieldDecoder (Oid OID.boolOid OID.array_boolOid) PGB.bool
instance FromSql Bool

instance FromSqlField Int16 where
    fromSqlField = FieldDecoder (Oid OID.int2Oid OID.array_int2Oid) PGB.int
instance FromSql Int16

instance FromSqlField Int32 where
    fromSqlField = FieldDecoder (Oid OID.int4Oid OID.array_int4Oid) PGB.int
instance FromSql Int32

instance FromSqlField Int64  where
    fromSqlField = FieldDecoder (Oid OID.int8Oid OID.array_int8Oid) PGB.int
instance FromSql Int64

instance FromSqlField Float where
    fromSqlField = FieldDecoder (Oid OID.float4Oid OID.array_float4Oid) PGB.float4
instance FromSql Float

instance FromSqlField Double where
    fromSqlField = FieldDecoder (Oid OID.float8Oid OID.array_float8Oid) PGB.float8
instance FromSql Double

instance FromSqlField Char where
    fromSqlField = FieldDecoder (Oid OID.charOid OID.array_charOid) PGB.char
instance FromSql Char where fromSql = notNull fromSqlField

instance {-# OVERLAPS #-} FromSqlField String where
    fromSqlField = FieldDecoder (Oid OID.textOid OID.array_textOid) (T.unpack <$> PGB.text_strict)
instance {-# OVERLAPS #-} FromSql String

instance FromSqlField Text where
    fromSqlField = FieldDecoder (Oid OID.textOid OID.array_textOid) PGB.text_strict
instance FromSql Text

instance FromSqlField TL.Text where
    fromSqlField = FieldDecoder (Oid OID.textOid OID.array_textOid) PGB.text_lazy
instance FromSql TL.Text

-- | If you want to encode some more specific Haskell type via JSON,
-- it is more efficient to use 'Data.Aeson.encode' and
-- 'PostgreSQL.Binary.Encoding.jsonb_bytes' directly, rather than this
-- instance.
instance FromSqlField ByteString where
    fromSqlField = FieldDecoder (Oid OID.byteaOid OID.array_byteaOid) (BS.copy <$> BP.remainders)
instance FromSql ByteString

instance FromSqlField BSL.ByteString where
    fromSqlField = FieldDecoder (Oid OID.byteaOid OID.array_byteaOid) (BSL.fromStrict . BS.copy <$> BP.remainders)
instance FromSql BSL.ByteString

-- TODO check for integer_datetimes setting
instance FromSqlField UTCTime where
    fromSqlField = FieldDecoder (Oid OID.timestamptzOid OID.array_timestamptzOid) PGB.timestamptz_int
instance FromSql UTCTime

instance FromSqlField Day where
    fromSqlField = FieldDecoder (Oid OID.dateOid OID.array_dateOid) PGB.date
instance FromSql Day

instance FromSqlField TimeOfDay where
    fromSqlField = FieldDecoder (Oid OID.timeOid OID.array_timeOid) PGB.time_int
instance FromSql TimeOfDay

instance FromSqlField TimeTZ where
    fromSqlField = FieldDecoder (Oid OID.timetzOid OID.array_timetzOid) (uncurry TimeTZ <$> PGB.timetz_int)
instance FromSql TimeTZ

instance FromSqlField UUID where
    fromSqlField = FieldDecoder (Oid OID.uuidOid OID.array_uuidOid) PGB.uuid
instance FromSql UUID

instance FromSqlField PQ.Oid where
    fromSqlField = PQ.Oid <$> FieldDecoder (Oid OID.oidOid OID.array_oidOid) PGB.int
instance FromSql PQ.Oid

instance FromSqlField PgName where
    fromSqlField = FieldDecoder (Oid OID.nameOid OID.array_nameOid) (PgName <$> PGB.text_strict)
instance FromSql PgName where fromSql = notNull fromSqlField

-- | If you want to encode some more specific Haskell type via JSON,
-- it is more efficient to use 'fromSqlJsonField' rather than this
-- instance.
instance FromSqlField JSON.Value where
    fromSqlField = FieldDecoder (Oid OID.jsonbOid OID.array_jsonbOid) PGB.jsonb_ast
instance FromSql JSON.Value

fromSqlJsonField :: JSON.FromJSON a => FieldDecoder a
fromSqlJsonField = FieldDecoder (Oid OID.jsonbOid OID.array_jsonbOid)
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

-- The array instances all overlap, because arrays can nest arbitrarily.
-- Unfortunately, this means a runtime error if an application programmer try to
-- nest arrays deeper than the instances that we provide.  So we provide more
-- instances than we expect users to want.

arrayDecoder :: FromSqlField a => (PGB.Array a -> PGB.Array b) -> FieldDecoder b
arrayDecoder  dims =
  FieldDecoder arrayType (PGB.array (dims (PGB.valueArray parser)))
  where
    FieldDecoder oneType parser = fromSqlField
    arrayType = case oneType of
      Oid _ oid -> Oid oid (PQ.Oid 0)
      TypeName name -> TypeName ("_" <> name)

dimV :: PGB.Array a -> PGB.Array (Vector a)
dimV = PGB.dimensionArray V.replicateM

dimL :: PGB.Array a -> PGB.Array [a]
dimL = PGB.dimensionArray replicateM

instance FromSqlField (Vector a) => FromSql (Vector a)
instance {-# OVERLAPPABLE #-} FromSqlField a => FromSqlField (Vector a) where
  fromSqlField = arrayDecoder dimV

instance {-# OVERLAPPABLE #-} FromSqlField a => FromSqlField (Vector (Vector a)) where
  fromSqlField = arrayDecoder (dimV . dimV)

instance {-# OVERLAPPABLE #-} FromSqlField a => FromSqlField (Vector (Vector (Vector a))) where
  fromSqlField = arrayDecoder (dimV . dimV . dimV)

instance {-# OVERLAPPABLE #-} FromSqlField a => FromSqlField (Vector (Vector (Vector (Vector a)))) where
  fromSqlField = arrayDecoder (dimV . dimV . dimV . dimV)

instance {-# OVERLAPPABLE #-} FromSqlField a => FromSqlField (Vector (Vector (Vector (Vector (Vector a))))) where
  fromSqlField = arrayDecoder (dimV . dimV . dimV . dimV . dimV)

instance FromSqlField [a] => FromSql [a]
instance {-# OVERLAPPABLE #-} FromSqlField a => FromSqlField [a] where
  fromSqlField = arrayDecoder dimL

instance {-# OVERLAPPABLE #-} FromSqlField a => FromSqlField [[a]] where
  fromSqlField = arrayDecoder (dimL . dimL)

instance {-# OVERLAPPABLE #-} FromSqlField a => FromSqlField [[[a]]] where
  fromSqlField = arrayDecoder (dimL . dimL . dimL)

instance {-# OVERLAPPABLE #-} FromSqlField a => FromSqlField [[[[a]]]] where
  fromSqlField = arrayDecoder (dimL . dimL . dimL . dimL)

instance {-# OVERLAPPABLE #-} FromSqlField a => FromSqlField [[[[[a]]]]] where
  fromSqlField = arrayDecoder (dimL . dimL . dimL . dimL . dimL)

instance (FromSqlField a, FromSqlField b) => FromSqlField (Tuple (a, b)) where
  fromSqlField =
    let vc = valueComposite
    in FieldDecoder (Oid OID.recordOid OID.array_recordOid)
        (Tuple <$> composite 2 (pure (,) <*> vc fromSqlField <*> vc fromSqlField))
instance (FromSqlField a, FromSqlField b) => FromSql (Tuple (a, b))

$(concat <$> traverse deriveFromSqlFieldTuple [3..maxTupleSize])
