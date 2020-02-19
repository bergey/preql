{-# HLINT ignore "Use camelCase" #-}

{-# LANGUAGE DeriveFunctor     #-}
-- | Decoding values from Postgres wire format to Haskell.

module Wire.FromSql where

import           Control.Applicative
import           Control.Applicative.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.ByteString (ByteString)
import           Data.Functor
import           Data.IORef
import           Data.Int
import           Data.Text (Text)

import qualified BinaryParser as BP
import qualified Data.ByteString as BS
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as OID
import qualified PostgreSQL.Binary.Decoding as PGB

data FieldDecoder a = FieldDecoder PQ.Oid (BP.BinaryParser a)
    deriving Functor

-- TODO RowDecoder should also have access to column name lookups.
data RowDecoder a = RowDecoder [PQ.Oid] (BP.BinaryParser a)
    deriving Functor

-- TODO better name
singleFieldDecoder :: FieldDecoder a -> RowDecoder a
singleFieldDecoder (FieldDecoder oid parser) = RowDecoder [oid] parser

instance Applicative RowDecoder where
    pure a = RowDecoder [] (pure a)
    RowDecoder t1 p1 <*> RowDecoder t2 p2 = RowDecoder (t1 <> t2) (p1 <*> p2)

runDecoder :: RowDecoder a -> PQ.Result -> PQ.Row -> IO (Either Text a)
runDecoder (RowDecoder _ parsers) result row = do
    columnRef <- newIORef (PQ.Col 0)
    parseFields result row columnRef parsers

parseFields :: PQ.Result -> PQ.Row -> IORef PQ.Column -> BP.BinaryParser a -> IO (Either Text a)
parseFields result currentRow columnRef parser = do
    currentColumn <- atomicModifyIORef' columnRef (\col -> (succ col, col))
    bytes <- PQ.getvalue result currentRow currentColumn
    let a = BP.run parser =<< emptyValue bytes
    return a

emptyValue :: Maybe a -> Either Text a
emptyValue = maybe (Left "Got empty value string from Postgres") Right

-- TODO more informative return type – collect errors, or OK
checkTypes :: RowDecoder a -> PQ.Result -> IO Bool
checkTypes (RowDecoder oids _) result = do
    ntuples <- PQ.ntuples result
    go (PQ.Col 0) oids where
        go _ [] = return True
        go col (expected : rest) = do
            actual <- PQ.ftype result col
            if expected == actual
                then go (succ col) rest
                else return False

class FromSqlField a where
    fromSqlField :: FieldDecoder a

instance FromSqlField Int32 where
    fromSqlField = FieldDecoder OID.int4Oid PGB.int

instance FromSqlField Int64  where
    fromSqlField = FieldDecoder OID.int8Oid PGB.int

instance FromSqlField Float where
    fromSqlField = FieldDecoder OID.float4Oid PGB.float4

instance FromSqlField Double where
    fromSqlField = FieldDecoder OID.float8Oid PGB.float8

instance FromSqlField Text where
    fromSqlField = FieldDecoder OID.textOid PGB.text_strict

class FromSql a where
    fromSql :: RowDecoder a

-- TODO support tuples of Rows, also

instance (FromSqlField a, FromSqlField b) => FromSql (a, b) where
    fromSql = (,) <$> singleFieldDecoder fromSqlField <*> singleFieldDecoder fromSqlField

instance (FromSqlField a, FromSqlField b, FromSqlField c) => FromSql (a, b, c) where
    fromSql = (,,) <$> singleFieldDecoder fromSqlField <*> singleFieldDecoder fromSqlField <*> singleFieldDecoder fromSqlField

-- -- TODO more tuple instances
-- -- TODO TH to make this less tedious
