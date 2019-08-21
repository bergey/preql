{-# LANGUAGE DeriveFunctor #-}
-- | Decoding values from Postgres wire format to Haskell.

module FromSql where

import           Control.Applicative.Free
import           Control.Monad.Trans.Except
import           Data.Attoparsec.ByteString (Parser)
import           Data.ByteString (ByteString)
import           Data.IORef
import           Data.Int

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


int32 :: SqlDecoder Int32
int32 = SqlDecoder [PQ.Oid 23] (liftAp P8.decimal)
