{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
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

import Data.Data
import Data.List (intersperse)
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift(..))
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

parensIf :: Bool -> B.Builder -> B.Builder
parensIf cond inner = if cond then parens inner else inner

spaceAfter :: B.Builder -> B.Builder
spaceAfter = (<> " ")

class FormatSql a where
    fmt :: a -> B.Builder
    fmt = fmtPrec 0

    fmtPrec :: Int -> a -> B.Builder
    fmtPrec _ = fmt

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

-- TODO replace all calls to @opt@ with @opt'@, rename
opt :: FormatSql a => B.Builder -> Maybe a -> B.Builder
opt _ Nothing = ""
opt prepend (Just a) = prepend <> fmt a

opt' :: FormatSql a => B.Builder -> Int -> Maybe a -> B.Builder
opt' _ _ Nothing = ""
opt' prepend p (Just a) = prepend <> fmtPrec p a

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
    fmtPrec _ (Lit lit)  = fmt lit
    fmtPrec _ (CRef name) = fmt name
    fmtPrec _ (NumberedParam i) = B.fromString ('$' : show i)
    fmtPrec _ (HaskellParam txt) = "${" <> B.fromText txt <> "}"
    fmtPrec p (BinOp op l r) = let (assoc, p1) = binOpPrec op
      in parensIf (p > p1) $ case assoc of
          LeftAssoc -> fmtPrec p1 l <> " " <> fmt op <> " " <> fmtPrec (p1 + 1) r
          RightAssoc -> fmtPrec (p1 + 1) l <> " " <> fmt op <> " " <> fmtPrec p1 r
          NonAssoc -> fmtPrec (p1 + 1) l <> " " <> fmt op <> " " <> fmtPrec (p1 + 1) r
    fmtPrec p (Unary op expr) = case op of
        Negate -> parensIf (p > 15) ("-" <>  fmtPrec 15 expr)
        Not -> parensIf (p > 5) ("NOT " <> fmtPrec 5 expr)
        IsNull -> parensIf (p > 7) (fmtPrec 8 expr <> " IS NULL")
        NotNull -> parensIf (p > 7) (fmtPrec 8 expr <> " IS NOT NULL")
    -- This looks funky, but seems to match the parser
    fmtPrec _ (Indirection e indirects) =
      let m_parens = case e of
            NumberedParam _ -> id
            CRef _ -> id
            _ -> parens
      in m_parens (fmt e) <> fmtIndirections indirects
    fmtPrec _ (SelectExpr stmt) = parens (fmt stmt)
    fmtPrec p (L likeE) = fmtPrec p likeE
    fmtPrec _ (Fun f) = fmt f
    fmtPrec _ (Cas c) = fmt c

fmtIndirections :: Foldable f => f Indirection -> TLB.Builder
fmtIndirections = foldMap (("." <>) . fmt)

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
        And -> "AND"
        Or -> "OR"

data Assoc = LeftAssoc | RightAssoc | NonAssoc
  deriving (Show, Eq, Enum, Bounded, Data, Lift, Generic)

binOpPrec :: BinOp -> (Assoc, Int)
binOpPrec op = case op of
  Or -> (LeftAssoc, 3)
  And -> (LeftAssoc, 4)
  IsDistinctFrom -> (NonAssoc, 7)
  IsNotDistinctFrom -> (NonAssoc, 7)
  Eq -> (NonAssoc, 8)
  LT -> (NonAssoc, 8)
  LTE -> (NonAssoc, 8)
  GT -> (NonAssoc, 8)
  GTE -> (NonAssoc, 8)
  NEq -> (NonAssoc, 8)
  Add -> (LeftAssoc, 12)
  Sub -> (LeftAssoc, 12)
  Mul -> (LeftAssoc, 13)
  Div -> (LeftAssoc, 13)
  Mod -> (LeftAssoc, 13)
  Exponent -> (LeftAssoc, 14)

setOpPrec :: SetOp -> Int
setOpPrec op = case op of
  Union -> 1
  Except -> 1
  Intersect -> 2

instance FormatSql LikeE where
  -- Expr L puts parens around if needed
    fmtPrec p LikeE{op, string, likePattern, escape, invert} = parensIf (p > likePrec) $
        fmtPrec 10 string <> (if invert then " NOT" else "")
        <> op' <> fmtPrec 10 likePattern <> opt' " ESCAPE " 10 escape
      where
        likePrec = if invert then 5 else 9
        op' = case op of
              Like -> " LIKE "
              ILike -> " ILIKE "
              Similar -> " SIMILAR TO "

instance FormatSql SelectStmt where
    fmtPrec _ (SelectValues values) = "VALUES " <> commas (fmap (parens . commas) values)
    fmtPrec _ (Simple un) = fmt un
    fmtPrec p (S ss so) = let topLevel = parensIf (p > 0) (fmtPrec 1 ss <> fmt so) in
      case withClause so of
        Nothing -> topLevel
        Just ctes -> fmt ctes <> " " <> topLevel
    fmtPrec p (Set op distinct l r) = parensIf (p > q) $
      fmtPrec q l <> " " <> fmt op <> d <> fmtPrec (q + 1) r
      where
        q = setOpPrec op
        d = case distinct of
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
    fmt SelectOptions{sortBy, offset, limit, locking} =
        optList " ORDER BY " sortBy
        <> " " <> spaces locking -- no commas
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
    fmtPrec p (J jt) = fmtPrec p jt
    fmtPrec p (As jt alias) = parensIf (p > 1) $ fmtPrec 1 jt <> " AS " <> fmt alias
    fmtPrec _ (SubSelect stmt alias) = parens (fmt stmt) <> " AS " <> fmt alias

instance FormatSql Alias where
    fmt (Alias name []) = fmt name
    fmt (Alias name columns) = fmt name <> parens (commas columns)

instance FormatSql JoinedTable where
    fmtPrec _ (Table name) = fmt name
    fmtPrec p (CrossJoin l r) = parensIf (p > 0) $ fmtPrec 0 l <> " CROSS JOIN " <> fmtPrec 1 r
    fmtPrec p (Join Inner Natural l r) = parensIf (p > 0) $ fmtPrec 0 l <> " NATURAL JOIN " <> fmtPrec 1 r
    fmtPrec p (Join ty Natural l r) = parensIf (p > 0) $ fmtPrec 0 l <> " NATURAL" <> fmt ty <> "JOIN " <> fmtPrec 1 r
    fmtPrec p (Join ty (Using cols) l r) = parensIf (p > 0) $ fmtPrec 0 l <> fmt ty <> " JOIN " <> fmtPrec 1 r <> " USING " <> parens (commas cols)
    fmtPrec p (Join ty (On expr) l r) = parensIf (p > 0) $ fmtPrec 0 l <> fmt ty <> " JOIN " <> fmtPrec 0 r <> " ON " <> fmtPrec 0 expr

instance FormatSql JoinType where
    fmt Inner = " INNER "
    fmt LeftJoin = " LEFT "
    fmt RightJoin = " RIGHT "
    fmt Full = " FULL "

instance FormatSql DistinctClause where
    fmt DistinctAll = "DISTINCT"
    fmt (DistinctOn expr) = "DISTINCT ON " <> parens (commas expr)

instance FormatSql SortBy where
    fmt (SortBy expr order nulls) = fmt expr <> " " <> fmt order <> fmt nulls

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
        " " <> fmt strength <> optList " OF " tables <> " " <> fmt wait

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
        Just (Window (Just alias) _ _ _) -> "OVER " <> fmt alias
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
   "CASE" <> opt " " implicitArg <> whenClauses' <> opt " ELSE " elseClause <> " END"
    where whenClauses' = spaces [ " WHEN " <> fmt condition <> " THEN " <> fmt result
                                | (condition, result) <- whenClause ]
