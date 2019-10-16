{-# HLINT ignore "Use camelCase" #-}

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Decoding values from Postgres wire format to Haskell.

module FromSql where

import           Control.Applicative
import           Control.Applicative.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Attoparsec.ByteString                 (Parser)
import           Data.ByteString                            (ByteString)
import           Data.Functor
import           Data.Int
import           Data.IORef

import qualified Data.Attoparsec.ByteString                 as P
import qualified Data.Attoparsec.ByteString.Char8           as P8
import qualified Data.ByteString                            as BS
import qualified Database.PostgreSQL.LibPQ                  as PQ
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as OID

data SqlDecoder a = SqlDecoder [PQ.Oid] (Ap Parser a)
    deriving Functor

instance Applicative SqlDecoder where
    pure a = SqlDecoder [] (pure a)
    SqlDecoder t1 p1 <*> SqlDecoder t2 p2 = SqlDecoder (t1 <> t2) (p1 <*> p2)

runDecoder :: SqlDecoder a -> PQ.Result -> PQ.Row -> ExceptT String IO a
runDecoder (SqlDecoder _ parsers) result row = do
    columnRef <- lift $ newIORef (PQ.Col 0)
    runAp (parseFields result row columnRef) parsers

parseFields :: PQ.Result -> PQ.Row -> IORef PQ.Column -> Parser a -> ExceptT String IO a
parseFields result currentRow columnRef parser = ExceptT $ do
    currentColumn <- atomicModifyIORef' columnRef (\col -> (succ col, col))
    bytes <- PQ.getvalue result currentRow currentColumn
    let a = P.parseOnly parser =<< emptyValue bytes
    return a

emptyValue :: Maybe a -> Either String a
emptyValue = maybe (Left "Got empty value string from Postgres") Right

-- TODO more informative return type – collect errors, or OK
checkTypes :: SqlDecoder a -> PQ.Result -> IO Bool
checkTypes (SqlDecoder oids _) result = do
    ntuples <- PQ.ntuples result
    go (PQ.Col 0) oids where
        go _ [] = return True
        go col (expected : rest) = do
            actual <- PQ.ftype result col
            if expected == actual
                then go (succ col) rest
                else return False

class FromSql a where
    fromSql :: SqlDecoder a

field :: PQ.Oid -> Parser a -> SqlDecoder a
field oid parser = SqlDecoder [oid] (liftAp parser)

instance FromSql Int32 where
    fromSql = field OID.int4Oid P8.decimal

instance FromSql Int64  where
    fromSql = field OID.int8Oid P8.decimal

instance FromSql Float where
    fromSql = field OID.float4Oid (realToFrac <$> pg_double)

instance FromSql Double where
    fromSql = field OID.float8Oid pg_double

instance (FromSql a, FromSql b) => FromSql (a, b) where
    fromSql = (,) <$> fromSql <*> fromSql

instance (FromSql a, FromSql b, FromSql c) => FromSql (a, b, c) where
    fromSql = (,,) <$> fromSql <*> fromSql <*> fromSql

-- TODO more tuple instances
-- TODO TH to make this less tedious

-- from Database.PostgreSQL.Simple.FromField
pg_double :: Parser Double
pg_double
    =   (P.string "NaN"       $> ( 0 / 0))
    <|> (P.string "Infinity"  $> ( 1 / 0))
    <|> (P.string "-Infinity" $> (-1 / 0))
    <|> P8.double
