{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Print the types in Syntax as valid SQL.  The emphasis is on
-- queries to send to the database, not on legibilty; no extra whitespace is introduced.

module Preql.QuasiQuoter.Syntax.Printer where

import Preql.Imports
import Preql.QuasiQuoter.Syntax.Name
import Preql.QuasiQuoter.Syntax.Syntax as Syn hiding (select)

import Data.List (intersperse)
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

spaceAfter :: B.Builder -> B.Builder
spaceAfter = (<> " ")

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
    fmt Null = "null"

instance FormatSql Statement where
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

unlessEmpty :: (B.Builder -> B.Builder) -> B.Builder -> B.Builder
unlessEmpty _ "" = ""
unlessEmpty f x = f x

optList :: FormatSql a => B.Builder -> [a] -> B.Builder
optList _ [] = ""
optList prepend as = prepend <> commas as

opt :: FormatSql a => B.Builder -> Maybe a -> B.Builder
opt _ Nothing = ""
opt prepend (Just a) = prepend <> fmt a

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
          Just conditions' -> " WHERE " <> fmt conditions'

instance FormatSql Setting where
    fmt (Setting column rhs) = fmt column <> "=" <> fmt rhs

instance FormatSql Update where
    fmt Update{table, settings, conditions} =
        "UPDATE " <> fmt table <> " SET " <> commas settings <> wh
      where wh = case conditions of
                Nothing         -> ""
                Just conditions' -> " WHERE " <> fmt conditions'

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
    fmt (Indirection e names) = parens (fmt e) <> foldMap (("." <>) . fmt) names
    fmt (SelectExpr stmt indirects) = parens (fmt stmt) <> fmtIndirections indirects
    fmt (And l r) = fmt l <> " AND " <> fmt r
    fmt (Or l r) = fmt l <> " OR " <> fmt r
    fmt (Not expr) = "NOT " <> fmt expr
    fmt (L likeE) = fmt likeE
    fmt (Fun f) = fmt f
    fmt (Cas c) = fmt c

instance FormatSql ColumnRef where
    fmt (ColumnRef name is) = fmt name <> fmtIndirections is

fmtIndirections :: [Indirection] -> TLB.Builder
fmtIndirections = mconcat . map (("." <>) . fmt)

instance FormatSql BinOp where
    fmt op = case op of
        Mul      -> "*"
        Div      -> "/"
        Add      -> "+"
        Sub      -> "-"
        Exponent -> "^"
        Mod -> "%"
        Eq    -> "="
        LT    -> "<"
        LTE   -> "<="
        GT    -> ">"
        GTE   -> ">="
        NEq   -> "!="
        IsDistinctFrom -> "IS DISTINCT FROM"
        IsNotDistinctFrom -> "IS NOT DISTINCT FROM"

instance FormatSql LikeE where
    fmt LikeE{op, string, likePattern, escape, invert} =
        fmt string <> if invert then " NOT" else ""
        <> op' <> fmt likePattern <> opt " ESCAPE" escape
      where op' = case op of
              Like -> " LIKE "
              ILike -> " ILIKE "
              Similar -> " SIMILAR TO "

instance FormatSql SelectStmt where
    fmt (SelectValues values) = "VALUES " <> commas (fmap (parens . commas) values)
    fmt (Simple un) = fmt un
    fmt (S ss so) = let topLevel = fmt ss <> fmt so in
      case withClause so of
        Nothing -> topLevel
        Just ctes -> fmt ctes <> " " <> topLevel
    fmt (Set op distinct l r) = fmt l <> " " <> fmt op <> d <> fmt r
      where d = case distinct of
                All -> " ALL "
                Distinct -> " "

instance FormatSql Select where
    fmt Select{targetList, from, distinct, whereClause, groupBy, having, window}
        = "SELECT " <> m_distinct <> commas (fmt <$> targetList) <> " FROM " <> commas from
          <> opt " WHERE " whereClause
          <> optList " GROUP BY " groupBy
          <> opt " HAVING " having
          <> optList " WINDOW " window
        where
          m_distinct = maybe "" (spaceAfter . fmt) distinct

instance FormatSql SelectOptions where
  -- ignore WithClause here; handle it in SelectStmt so we can put it before the top query
    fmt SelectOptions{sortBy, offset, limit, locking} = spaces locking -- no commas
        <> optList " ORDER BY " sortBy
        <> opt " LIMIT " limit
        <> opt " OFFSET " offset

instance FormatSql WithClause where
  fmt With {commonTables, recursive} =
    "WITH" <> recursive' <> commas commonTables
    where recursive' = case recursive of
            Recursive -> " RECURSIVE "
            NotRecursive -> " "

instance FormatSql Materialized where
  fmt Materialized = "MATERIALIZED"
  fmt NotMaterialized = "NOT MATERIALIZED"
  fmt MaterializeDefault = ""

instance FormatSql CTE where
  fmt CommonTableExpr {name, aliases, materialized, query} =
    fmt name <> unlessEmpty parens (commas aliases)
    <> unlessEmpty spacesAround (fmt materialized) <> parens (fmt query)
    where
      spacesAround s = " " <> s <> " "

instance FormatSql TableRef where
    fmt (Table name) = fmt name
    fmt (Aliased ref alias) = fmt ref <> " AS " <> fmt alias
    fmt (CrossJoin l r) = fmt l <> " CROSS JOIN " <> fmt r
    fmt (Join ty Natural l r) = fmt l <> " NATURAL" <> fmt ty <> fmt r
    fmt (Join ty (Using cols) l r) = fmt l <> fmt ty <> fmt r <> " USING " <> commas cols
    fmt (Join ty (On expr) l r) = fmt l <> fmt ty <> fmt r <> " ON " <> fmt expr
    fmt (SubSelect stmt alias) = parens (fmt stmt) <> " AS " <> fmt alias

instance FormatSql Alias where
    fmt (Alias name []) = fmt name
    fmt (Alias name columns) = fmt name <> parens (commas (fmt <$> columns))

instance FormatSql JoinType where
    fmt Inner = " INNER "
    fmt LeftJoin = " LEFT "
    fmt RightJoin = " RIGHT "
    fmt Full = " FULL "

instance FormatSql DistinctClause where
    fmt DistinctAll = "DISTINCT"
    fmt (DistinctOn expr) = "DISTINCT ON " <> parens (commas expr)

instance FormatSql SortBy where
    fmt (SortBy expr order nulls) = fmt expr <> fmt order <> fmt nulls

instance FormatSql SortOrderOrUsing where
    fmt (SortOrder order) = fmt order
    fmt (SortUsing op) = "USING " <> fmt op

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

instance FormatSql Locking where
    fmt Locking{strength, tables, wait} =
        fmt strength <> optList " OF " tables <> " " <> fmt wait

instance FormatSql LockingStrength where
    fmt ForUpdate = "FOR UPDATE"
    fmt ForNoKeyUpdate = "FOR NO KEY UPDATE"
    fmt ForShare = "FOR SHARE"
    fmt ForKeyShare = "FOR KEY SHARE"

instance FormatSql LockWait where
    fmt LockWaitError = "NOWAIT"
    fmt LockWaitSkip = "SKIP LOCKED"
    fmt LockWaitBlock = ""

instance FormatSql SetOp where
    fmt Union = "UNION"
    fmt Intersect = "INTERSECT"
    fmt Except = "EXCEPT"

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

instance FormatSql FunctionApplication where
  fmt FApp {..} = fmt name <> fmtIndirections indirection
    <> parens (distinct' <> fmt arguments <> sortBy') <> withinGroup'
    <> maybe "" (\fc -> " FILTER " <> parens ("WHERE " <> fmt fc)) filterClause
    <> over'
    where
      distinct' = if distinct then "DISTINCT " else ""
      sortBy' = if withinGroup then "" else optList "ORDER BY " sortBy
      withinGroup' = if withinGroup then optList "WITHIN GROUP " sortBy else ""
      over' = case over of
        Nothing -> ""
        Just (Window (Just alias) _ _ _ _) -> "OVER " <> fmt alias
        Just Window {refName, partitionClause, orderClause} -> "OVER " <> parens
              (opt "" refName
               <> optList " PARTITION BY " partitionClause
               <> optList " ORDER BY " orderClause)

instance FormatSql FunctionArguments where
  fmt StarArg = "*"
  fmt (A args) = commas args

instance FormatSql Argument where
  fmt (E e) = fmt e
  fmt (Named name e) = fmt name <> " => " <> fmt e

instance FormatSql Case where
  fmt Case { whenClause, implicitArg, elseClause } =
   "CASE" <> opt " " implicitArg <> whenClauses' <> opt " ELSE " elseClause
    where whenClauses' = spaces [ "WHEN " <> fmt condition <> " THEN " <> fmt result
                                | (condition, result) <- whenClause ]
