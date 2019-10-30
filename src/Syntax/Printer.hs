{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Print the types in Syntax as valid SQL.  The emphasis is on
-- queries to send to the database, not on legibilty; no extra whitespace is introduced.

module Syntax.Printer where

import           Syntax.Internal
import           Syntax.Untyped

import           Data.ByteString                  (ByteString)
import           Data.Foldable                    (toList)
import           Data.List
import           Data.Vector                      (Vector, (!?))
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

data Params = Placeholders | Params (Vector B.Builder)

emptyParams :: Params
emptyParams = Params mempty

class FormatSql a where
    fmt :: Params -> a -> B.Builder

formatAsString :: FormatSql a => Params -> a -> String
formatAsString ps = TL.unpack . TLB.toLazyText . fmt ps

formatAsByteString :: FormatSql a => Params -> a -> ByteString
formatAsByteString ps = T.encodeUtf8 . formatAsText ps

formatAsText :: FormatSql a => Params -> a -> T.Text
formatAsText ps = TL.toStrict . TLB.toLazyText . fmt ps

instance FormatSql Name where
    -- TODO enclose keywoards &c in double quotes
    fmt _ = B.fromText . getName

instance FormatSql Literal where
    fmt _ (I i)     = B.decimal i
    fmt _ (F x)     = B.realFloat x
    fmt _ (T t)     = quote (B.fromText t)
    fmt _ (B True)  = "true"
    fmt _ (B False) = "false"

instance FormatSql Query where
    fmt ps (QI insert) = fmt ps insert
    fmt ps (QD delete) = fmt ps delete
    fmt ps (QU update) = fmt ps update
    fmt ps (QS select) = fmt ps select

commas :: (FormatSql a, Foldable f) => Params -> f a -> B.Builder
commas ps as = mconcat (intersperse ", " (map (fmt ps) (toList as)))

instance FormatSql Insert where
    fmt ps Insert{table, columns, values} =
        "INSERT INTO " <> fmt ps table <> " (" <> commas ps columns <>
        ") VALUES (" <> commas ps values <> ")"

instance FormatSql Delete where
    fmt ps Delete{table, conditions} = "DELETE FROM " <> fmt ps table <> wh where
      wh = case conditions of
          Nothing         -> ""
          Just conditions -> " WHERE " <> fmt ps conditions

instance FormatSql Setting where
    fmt ps (Setting column rhs) = fmt ps column <> "=" <> fmt ps rhs

instance FormatSql Update where
    fmt ps Update{table, settings, conditions} =
        "UPDATE " <> fmt ps table <> " SET " <> commas ps settings <> wh
      where wh = case conditions of
                Nothing         -> ""
                Just conditions -> " WHERE " <> fmt ps conditions

instance FormatSql Select where
    fmt ps Select{table, columns, conditions} =
        "SELECT " <> commas ps columns <> " FROM " <> fmt ps table <> wh
      where wh = case conditions of
                Nothing         -> ""
                Just conditions -> " WHERE " <> fmt ps conditions

instance FormatSql Condition where
    fmt ps (Compare op column value) = fmt ps column <> " " <> fmt ps op <> " " <> fmt ps value
    fmt ps (Or l r) = parens (fmt ps l) <> " OR " <> parens (fmt ps r)
    fmt ps (And l r) = parens (fmt ps l) <> " AND " <> parens (fmt ps r)
    fmt ps (Not cond) = "NOT" <> parens (fmt ps cond)

instance FormatSql Expr where
    fmt ps (Lit lit)  = fmt ps lit
    fmt ps (Var name) = fmt ps name
    fmt ps (Param i) = case ps of
        Placeholders -> B.fromString ('$' : show i)
        Params ps -> case ps !? fromIntegral (i - 1) of
            Nothing -> error $ "parameter list to short for $" <> show i
            Just value -> value
    fmt ps (BinOp op l r) = "(" <> fmt ps l <> ") " <> fmt ps op <> " (" <> fmt ps r <> ")"
    fmt ps (Unary op expr) = case op of
        NegateNum  -> "-" <> parens (fmt ps expr)
        NegateBool -> "NOT " <> parens (fmt ps expr)
        IsNull     -> parens (fmt ps expr) <> " IS NULL"
        NotNull    -> parens (fmt ps expr) <> " IS NOT NULL"

instance FormatSql BinOp where
    fmt ps op = case op of
        Mul      -> "*"
        Div      -> "/"
        Add      -> "+"
        Sub      -> "-"
        Exponent -> "^"
        Comp c   -> fmt ps c

instance FormatSql Compare where
    fmt _ op = case op of
        Eq    -> "="
        LT    -> "<"
        LTE   -> "<="
        GT    -> ">"
        GTE   -> ">="
        NEq   -> "!="
        Like  -> "LIKE"
        ILike -> "ILIKE"
