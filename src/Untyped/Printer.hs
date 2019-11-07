{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Print the types in Syntax as valid SQL.  The emphasis is on
-- queries to send to the database, not on legibilty; no extra whitespace is introduced.

module Untyped.Printer where

import           Untyped.Name
import           Untyped.Syntax

import           Data.ByteString                  (ByteString)
import           Data.Foldable                    (toList)
import           Data.List
import           Prelude                          hiding (GT, LT, lex)

import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TLB
import qualified Data.Text.Lazy.Builder           as B
import qualified Data.Text.Lazy.Builder.Int       as B
import qualified Data.Text.Lazy.Builder.RealFloat as B

quote :: B.Builder -> B.Builder
quote s = "'" <> s <> "'"

doubleQuote :: B.Builder -> B.Builder
doubleQuote s = "\"" <> s <> "\""

parens :: B.Builder -> B.Builder
parens s = "(" <> s <> ")"

class FormatSql a where
    fmt :: a -> B.Builder

formatAsString :: FormatSql a => a -> String
formatAsString = TL.unpack . TLB.toLazyText . fmt

formatAsByteString :: FormatSql a => a -> ByteString
formatAsByteString = T.encodeUtf8 . formatAsText

formatAsText :: FormatSql a => a -> T.Text
formatAsText = TL.toStrict . TLB.toLazyText . fmt

instance FormatSql Name where
    -- TODO enclose keywoards &c in double quotes
    fmt = B.fromText . getName

instance FormatSql Literal where
    fmt (I i)     = B.decimal i
    fmt (F x)     = B.realFloat x
    fmt (T t)     = quote (B.fromText t)
    fmt (B True)  = "true"
    fmt (B False) = "false"

instance FormatSql Query where
    fmt (QI insert) = fmt insert
    fmt (QD delete) = fmt delete
    fmt (QU update) = fmt update
    fmt (QS select) = fmt select

commas :: (FormatSql a, Foldable f) => f a -> B.Builder
commas as = mconcat (intersperse ", " (map fmt (toList as)))

instance FormatSql Insert where
    fmt Insert{table, columns, values} =
        "INSERT INTO " <> fmt table <> " (" <> commas columns <>
        ") VALUES (" <> commas values <> ")"

instance FormatSql Delete where
    fmt Delete{table, conditions} = "DELETE FROM " <> fmt table <> wh where
      wh = case conditions of
          Nothing         -> ""
          Just conditions -> " WHERE " <> fmt conditions

instance FormatSql Setting where
    fmt (Setting column rhs) = fmt column <> "=" <> fmt rhs

instance FormatSql Update where
    fmt Update{table, settings, conditions} =
        "UPDATE " <> fmt table <> " SET " <> commas settings <> wh
      where wh = case conditions of
                Nothing         -> ""
                Just conditions -> " WHERE " <> fmt conditions

instance FormatSql Select where
    fmt Select{table, columns, conditions} =
        "SELECT " <> commas columns <> " FROM " <> fmt table <> wh
      where wh = case conditions of
                Nothing         -> ""
                Just conditions -> " WHERE " <> fmt conditions

instance FormatSql Condition where
    fmt (Compare op column value) = fmt column <> " " <> fmt op <> " " <> fmt value
    fmt (Or l r) = parens (fmt l) <> " OR " <> parens (fmt r)
    fmt (And l r) = parens (fmt l) <> " AND " <> parens (fmt r)
    fmt (Not cond) = "NOT" <> parens (fmt cond)

instance FormatSql Expr where
    fmt (Lit lit)  = fmt lit
    fmt (Var name) = fmt name
    fmt (NumberedParam i) = B.fromString ('$' : show i)
    fmt (InlineParam txt) = B.fromText txt
    fmt (BinOp op l r) = "(" <> fmt l <> ") " <> fmt op <> " (" <> fmt r <> ")"
    fmt (Unary op expr) = case op of
        NegateNum  -> "-" <> parens (fmt expr)
        NegateBool -> "NOT " <> parens (fmt expr)
        IsNull     -> parens (fmt expr) <> " IS NULL"
        NotNull    -> parens (fmt expr) <> " IS NOT NULL"

instance FormatSql BinOp where
    fmt op = case op of
        Mul      -> "*"
        Div      -> "/"
        Add      -> "+"
        Sub      -> "-"
        Exponent -> "^"
        Comp c   -> fmt c

instance FormatSql Compare where
    fmt op = case op of
        Eq    -> "="
        LT    -> "<"
        LTE   -> "<="
        GT    -> ">"
        GTE   -> ">="
        NEq   -> "!="
        Like  -> "LIKE"
        ILike -> "ILIKE"
