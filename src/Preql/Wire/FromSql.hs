{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# HLINT ignore "Use camelCase" #-}

{-# LANGUAGE DeriveFunctor     #-}
-- | Decoding values from Postgres wire format to Haskell.

module Preql.Wire.FromSql where

import            Preql.Wire.Internal

import           Control.Applicative.Free
import           Control.Monad.Except
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Int
import           Preql.Imports

import qualified BinaryParser as BP
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as OID
import qualified PostgreSQL.Binary.Decoding as PGB

data FieldDecoder a = FieldDecoder PQ.Oid (BP.BinaryParser a)
    deriving Functor

data DecoderError = FieldError (LocatedError FieldError) | PgTypeMismatch [TypeMismatch]
    deriving (Show, Eq, Typeable)
instance Exception DecoderError

data TypeMismatch = TypeMismatch
    { expected :: PQ.Oid
    , actual :: PQ.Oid
    , column :: PQ.Column
    , columnName :: Maybe Text
    } deriving (Eq, Show, Typeable)

throwLocated :: FieldError -> InternalDecoder a
throwLocated failure = do
    DecoderState{..} <- get
    throwError (LocatedError row column failure)

decodeVector :: RowDecoder a -> PQ.Result -> ExceptT DecoderError IO (Vector a)
decodeVector rd@(RowDecoder oids parsers) result = do
    mismatches <- fmap catMaybes $ for (zip [PQ.Col 0 ..] oids) $ \(column, expected) -> do
        actual <- liftIO $ PQ.ftype result column
        if actual == expected
            then return Nothing
            else do
                m_name <- liftIO $ PQ.fname result column
                let columnName = decodeUtf8With lenientDecode <$> m_name
                return $ Just (TypeMismatch{..})
    unless (null mismatches) (throwError (PgTypeMismatch mismatches))
    (PQ.Row ntuples) <- liftIO $ PQ.ntuples result
    let toRow = PQ.toRow . fromIntegral
    withExceptT FieldError $
        V.generateM (fromIntegral ntuples) (decodeRow rd result . toRow)

notNull :: FieldDecoder a -> RowDecoder a
notNull (FieldDecoder oid parser) = RowDecoder [oid] $ do
    m_bs <- getNextValue
    case m_bs of
        Nothing -> throwLocated UnexpectedNull
        Just bs -> either (throwLocated . ParseFailure) pure (BP.run parser bs)

nullable :: FieldDecoder a -> RowDecoder (Maybe a)
nullable (FieldDecoder oid parser) = RowDecoder [oid] $ do
    m_bs <- getNextValue
    case m_bs of
        Nothing -> return Nothing
        Just bs -> either (throwLocated . ParseFailure) (pure . Just) (BP.run parser bs)

class FromSqlField a where
    fromSqlField :: FieldDecoder a

class FromSql a where
    fromSql :: RowDecoder a

instance FromSqlField Int32 where
    fromSqlField = FieldDecoder OID.int4Oid PGB.int
instance FromSql Int32 where fromSql = notNull fromSqlField

instance FromSqlField Int64  where
    fromSqlField = FieldDecoder OID.int8Oid PGB.int
instance FromSql Int64 where fromSql = notNull fromSqlField

instance FromSqlField Float where
    fromSqlField = FieldDecoder OID.float4Oid PGB.float4
instance FromSql Float where fromSql = notNull fromSqlField

instance FromSqlField Double where
    fromSqlField = FieldDecoder OID.float8Oid PGB.float8
instance FromSql Double where fromSql = notNull fromSqlField

instance FromSqlField Text where
    fromSqlField = FieldDecoder OID.textOid PGB.text_strict
instance FromSql Text where fromSql = notNull fromSqlField

instance FromSqlField ByteString where
    fromSqlField = FieldDecoder OID.byteaOid (BS.copy <$> BP.remainders)
instance FromSql ByteString where fromSql = notNull fromSqlField

-- Overlappable so applications can write Maybe for multi-field domain types
instance {-# OVERLAPPABLE #-} FromSqlField a => FromSql (Maybe a) where
    fromSql = nullable fromSqlField

instance (FromSql a, FromSql b) => FromSql (a, b) where
    fromSql = (,) <$> fromSql <*> fromSql

instance (FromSql a, FromSql b, FromSql c) => FromSql (a, b, c) where
    fromSql = (,,) <$> fromSql <*> fromSql <*> fromSql

instance (FromSql a, FromSql b, FromSql c, FromSql d) => FromSql (a, b, c, d) where
    fromSql = (,,,) <$> fromSql <*> fromSql <*> fromSql <*> fromSql

instance (FromSql a, FromSql b, FromSql c, FromSql d, FromSql e) => FromSql (a, b, c, d, e) where
    fromSql = (,,,,) <$> fromSql <*> fromSql <*> fromSql <*> fromSql <*> fromSql

-- -- TODO more tuple instances
-- -- TODO TH to make this less tedious
