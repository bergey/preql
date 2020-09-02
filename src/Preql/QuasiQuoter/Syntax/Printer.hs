{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Print the types in Syntax as valid SQL.  The emphasis is on
-- queries to send to the database, not on legibilty; no extra whitespace is introduced.

module Preql.QuasiQuoter.Syntax.Printer where

import Preql.Imports
import Preql.QuasiQuoter.Syntax.Name
import Preql.QuasiQuoter.Syntax.Syntax

import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.List
import Prelude hiding (GT, LT, lex)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as B
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
commas = fmtList ", "

spaces :: (FormatSql a, Foldable f) => f a -> B.Builder
spaces = fmtList " "

fmtList :: (FormatSql a, Foldable f) => B.Builder -> f a -> B.Builder
fmtList sep as = mconcat (intersperse sep (map fmt (toList as)))

instance FormatSql B.Builder where
    fmt = id

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

instance FormatSql Condition where
    fmt (Compare op column value) = fmt column <> " " <> fmt op <> " " <> fmt value
    fmt (Or l r) = parens (fmt l) <> " OR " <> parens (fmt r)
    fmt (And l r) = parens (fmt l) <> " AND " <> parens (fmt r)
    fmt (Not cond) = "NOT" <> parens (fmt cond)

instance FormatSql Expr where
    fmt (Lit lit)  = fmt lit
    fmt (CRef name) = fmt name
    fmt (NumberedParam i indirect) = B.fromString ('$' : show i) <> fmtIndirections indirect
    fmt (HaskellParam txt) = "${" <> B.fromText txt <> "}"
    fmt (BinOp op l r) = "(" <> fmt l <> ") " <> fmt op <> " (" <> fmt r <> ")"
    fmt (Unary op expr) = case op of
        NegateNum  -> "-" <> parens (fmt expr)
        NegateBool -> "NOT " <> parens (fmt expr)
        IsNull     -> parens (fmt expr) <> " IS NULL"
        NotNull    -> parens (fmt expr) <> " IS NOT NULL"
    fmt (SelectExpr stmt indirects) = parens (fmt stmt) <> fmtIndirections indirects
    fmt (AndE l r) = fmt l <> " AND " <> fmt r
    fmt (OrE l r) = fmt l <> " OR " <> fmt r
    fmt (NotE expr) = "NOT " <> fmt expr

fmtIndirections :: [Indirection] -> TLB.Builder
fmtIndirections = mconcat . map (("." <>) . fmt)

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

instance FormatSql SelectStmt where
    fmt (SelectValues values) = "VALUES " <> commas (fmap (parens . commas) values)
    fmt (SelectUnordered un) = fmt un
    fmt (SortedSelect select sortBy) = fmt select <> " ORDER BY " <> commas sortBy

instance FormatSql Unordered where
    fmt Unordered{targetList, from, whereClause, groupBy, having, window}
        = "SELECT " <> m_distinct <> commas (fmt <$> targetList) <> " FROM " <> commas (fmt <$> from) <>
          m_where <> m_groupBy <> m_having <> m_window
        where
          m_distinct = "" -- TODO
          m_where = case whereClause of
              Nothing -> ""
              Just expr -> " WHERE " <> fmt expr
          m_groupBy = case groupBy of
              [] -> ""
              _ -> " GROUP BY " <> commas (fmt <$> groupBy)
          m_having = case having of
              Nothing -> ""
              Just expr -> " HAVING " <> fmt expr
          m_window = case window of
              [] -> ""
              _ -> " WINDOW " <> commas (fmt <$> window)

instance FormatSql TableRef where
    fmt TableRef {relation, alias} = fmt relation <> m_alias where
      m_alias = case alias of
          Nothing -> ""
          Just (Alias name columns) -> " AS " <> fmt name <> case columns of
              [] -> ""
              _ -> " " <> parens (commas (fmt <$> columns))

instance FormatSql SortBy where
    fmt (SortBy expr order nulls) = fmt expr <> fmt order <> fmt nulls

instance FormatSql SortOrderOrUsing where
    fmt (SortOrder order) = fmt order
    fmt (Using op) = "USING " <> fmt op

instance FormatSql SortOrder where
    -- leading space
    fmt Ascending = " ASC"
    fmt Descending = " DESC"
    fmt DefaultSortOrder = ""

instance FormatSql NullsOrder where
    -- leading space
    fmt NullsFirst = " NULLS FIRST"
    fmt NullsLast = " NULLS LAST"
    fmt NullsOrderDefault = ""

instance FormatSql ResTarget where
    fmt Star = "*"
    fmt (Column expr Nothing) = fmt expr
    fmt (Column expr (Just name)) = fmt expr <> " AS " <> fmt name

-- instance FormatSql ColumnRef where
--     fmt ColumnRef {value, name} = fmt value <> case name of
--         Nothing -> ""
--         Just n -> "." <> fmt n

instance FormatSql Window where
    fmt Window {name, refName, partitionClause, orderClause}
        = m_name <> " AS (" <> mconcat [m_refName, m_partition, m_order ] <> ")" where
      m_name = case name of
          Nothing -> error "parser should never give a nameless Window"
          Just n -> fmt n
      m_refName = maybe "" fmt refName
      m_partition = case partitionClause of
          [] -> ""
          _ -> " PARTITION BY " <> commas (fmt <$> partitionClause)
      m_order = case orderClause of
          [] -> ""
          _ -> " ORDER BY " <> commas (fmt <$> orderClause)
