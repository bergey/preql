{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Decoding values from Postgres wire format to Haskell.

module FromSql where

import           Control.Applicative
import           Control.Applicative.Free
import           Control.Monad.Trans.Except
import           Data.Attoparsec.ByteString (Parser)
import           Data.ByteString (ByteString)
import           Data.IORef
import           Data.Int

import qualified Database.PostgreSQL.Simple.TypeInfo.Static as OID
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import qualified Data.ByteString as BS
import qualified Database.PostgreSQL.LibPQ as PQ

data SqlDecoder a = SqlDecoder [PQ.Oid] (Ap Parser a)
    deriving Functor

instance Applicative SqlDecoder where
    pure a = SqlDecoder [] (pure a)
    SqlDecoder t1 p1 <*> SqlDecoder t2 p2 = SqlDecoder (t1 <> t2) (p1 <*> p2)

runDecoder :: SqlDecoder a -> PQ.Result -> PQ.Row -> IO (Either String a)
runDecoder (SqlDecoder _ parsers) result row = do
    columnRef <- newIORef (PQ.Col 0)
    runExceptT $ runAp (parseFields result row columnRef) parsers

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

field :: PQ.Oid -> Parser a -> SqlDecoder a
field oid parser = SqlDecoder [oid] (liftAp parser)

int32 :: SqlDecoder Int32
int32 = field OID.int4Oid P8.decimal

int64 :: SqlDecoder Int64
int64 = field OID.int8Oid P8.decimal

float32 :: SqlDecoder Float
float32 = field OID.float4Oid (realToFrac <$> pg_double)

float64 :: SqlDecoder Double
float64 = field OID.float8Oid pg_double

-- from Database.PostgreSQL.Simple.FromField
pg_double :: Parser Double
pg_double
    =   (P.string "NaN"       *> pure ( 0 / 0))
    <|> (P.string "Infinity"  *> pure ( 1 / 0))
    <|> (P.string "-Infinity" *> pure (-1 / 0))
    <|> P8.double
