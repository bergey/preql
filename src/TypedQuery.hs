{-# LANGUAGE ScopedTypeVariables #-}

module TypedQuery where

import           PQResultUtils
import           Untyped.Printer
import           Untyped.Syntax                      (Query)

import           Data.Int                            (Int64)
import           Data.String

import           Data.Text.Encoding                  (decodeUtf8, encodeUtf8)

-- import qualified Data.ByteString                     as BS
-- import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder             as BS
import qualified Data.ByteString.Lazy                as BSL
import qualified Data.Text.Lazy.Builder              as B
import qualified Data.Vector                         as V
-- import qualified Database.PostgreSQL.LibPQ           as PQ
import qualified Database.PostgreSQL.Simple          as PS
import qualified Database.PostgreSQL.Simple.FromRow  as PS
import qualified Database.PostgreSQL.Simple.Internal as PS
import qualified Database.PostgreSQL.Simple.ToField  as PS
import qualified Database.PostgreSQL.Simple.ToRow    as PS

-- | An SQL query, annotated with the types of its parameters and results.
data TypedQuery p r = TypedQuery String Query
    deriving Show

buildActions :: PS.Connection -> PS.Query -> [PS.Action] -> IO Params
buildActions conn q row =
    Params . fmap toTextBuilder <$> traverse (PS.buildAction conn q row) (V.fromList row)
    where
      toTextBuilder :: BS.Builder -> B.Builder
      toTextBuilder = B.fromText . decodeUtf8 . BSL.toStrict . BS.toLazyByteString

-- | compare @query conn q p@
query :: (PS.ToRow p, PS.FromRow r) => PS.Connection -> TypedQuery p r -> p -> IO [r]
query conn (TypedQuery raw parsed) p = do
    let q = fromString raw :: PS.Query
    substitutions <- buildActions conn q (PS.toRow p)
    let formatted = formatAsByteString substitutions parsed
    result <- PS.exec conn formatted
    finishQueryWith PS.fromRow conn q result

-- | compare @query conn q p@
query_ :: PS.FromRow r => PS.Connection -> TypedQuery p r -> IO [r]
query_ conn (TypedQuery raw parsed) = do
    let q = fromString raw :: PS.Query
    let formatted = formatAsByteString emptyParams parsed
    result <- PS.exec conn formatted
    finishQueryWith PS.fromRow conn q result

execute :: PS.ToRow p => PS.Connection -> TypedQuery p () -> p -> IO Int64
execute conn (TypedQuery raw parsed) p = do
    let
        row = PS.toRow p
        q = fromString raw :: PS.Query
    substitutions <- buildActions conn q row
    let formatted = formatAsByteString substitutions parsed
    result <- PS.exec conn formatted
    PS.finishExecute conn q result
