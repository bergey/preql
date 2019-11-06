{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypedQuery where

import Data.Int (Int64)
import Database.PostgreSQL.Simple

-- | An SQL query, annotated with the types of its parameters and results.
newtype TypedQuery p r = TypedQuery Query
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

execute :: PS.ToRow p => PS.Connection -> TypedQuery p () -> p -> IO Int64
execute conn (TypedQuery raw parsed) p = do
    let
        row = PS.toRow p
        q = fromString raw :: PS.Query
    substitutions <- buildActions conn q row
    let formatted = formatAsByteString substitutions parsed
    result <- PS.exec conn formatted
    PS.finishExecute conn q result
