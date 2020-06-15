-- | An Enum type to test Postgres typname lookup.

module Test.Wire.Enum where

import Preql.Wire

import Text.Read (readMaybe)
import qualified BinaryParser as BP
import qualified Data.Text as T
import qualified PostgreSQL.Binary.Decoding as PGB

data MyEnum = A | B | C
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

instance FromSqlField MyEnum where
    fromSqlField = FieldDecoder (TypeName "my_enum") $ do
        text <- PGB.text_strict
        case readMaybe (T.unpack text) of
            Just x -> return x
            Nothing -> BP.failure ("could not parse as MyEnum: " <> text)
instance FromSql MyEnum where fromSql = notNull fromSqlField
