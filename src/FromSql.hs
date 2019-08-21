{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Decoding values from Postgres wire format to Haskell.

module FromSql where

import Data.Int
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString (Parser)

import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8

newtype SqlDecoder a = SqlDecoder (Parser a)
    deriving (Functor, Applicative)
-- TODO wrong Applicative instance!  Implies sequencing, rather than independent columns.
-- Maybe Free Applicative is what I want here, instead?  Only use the Functor from Parser, not the Applicative.
-- https://hackage.haskell.org/package/free-5.1.1/docs/Control-Applicative-Free.html

int32 :: SqlDecoder Int32
int32 = SqlDecoder P8.decimal

parse :: SqlDecoder a -> ByteString -> Either String a
parse (SqlDecoder p) = P.parseOnly p
