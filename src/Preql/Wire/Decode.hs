{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Decoding values from Postgres wire format to Haskell.

module Preql.Wire.Decode where

import Preql.FromSql.Class
import Preql.Wire.Errors
import Preql.Wire.Internal
import Preql.Wire.Types

import Control.Monad.Except
import Control.Monad.Trans.State
import Data.Int
import GHC.TypeNats
import Preql.Imports

import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Preql.Wire.TypeInfo.Static as OID

decodeVector :: KnownNat n =>
    (PgType -> IO (Either QueryError PQ.Oid)) -> RowDecoder n a -> PQ.Result -> IO (Either QueryError (Vector a))
decodeVector lookupType rd@(RowDecoder pgtypes _parsers) result = do
    mismatches <- fmap (catMaybes . VS.toList) $ for (VS.zip (VS.enumFromN 0) pgtypes) $ \(column@(PQ.Col cint), expected) -> do
        actual <- PQ.ftype result column
        e_expectedOid <- lookupType expected
        case e_expectedOid of
            Right oid | actual == oid -> return Nothing
            _ -> do
                m_name <- liftIO $ PQ.fname result column
                let columnName = decodeUtf8With lenientDecode <$> m_name
                return $ Just (TypeMismatch{column = fromIntegral cint, ..})
    if not (null mismatches)
        then return (Left (PgTypeMismatch mismatches))
        else do
            (PQ.Row ntuples) <- liftIO $ PQ.ntuples result
            let toRow = PQ.toRow . fromIntegral
            fmap (first DecoderError) . runExceptT $
                V.generateM (fromIntegral ntuples) (decodeRow rd result . toRow)
