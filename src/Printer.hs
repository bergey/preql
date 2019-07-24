{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Print the types in Syntax as valid SQL.  The emphasis is on
-- queries to send to the database, not on legibilty; no extra whitespace is introduced.

module Printer where

import           Internal
import           Syntax

import           Data.Foldable (toList)
import           Data.List

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B

quote :: B.Builder -> B.Builder
quote s = "'" <> s <> "'"

class FormatSql a where
    fmt :: a -> B.Builder

instance FormatSql Name where
    fmt = B.fromText . getName

instance FormatSql Literal where
    fmt (I i) = B.decimal i
    fmt (F x) = B.realFloat x
    fmt (T t) = quote (B.fromText t)
    fmt (B True) = "true"
    fmt (B False) = "false"

commas :: (FormatSql a, Foldable f) => f a -> B.Builder
commas as = mconcat (intersperse ", " (map fmt (toList as)))

instance FormatSql Insert where
    fmt Insert{table, columns, values} =
        "INSERT INTO " <> fmt table <> " (" <> commas columns <>
        ") VALUES (" <> commas values <> ")"

instance FormatSql Delete where
    fmt Delete{table, conditions} = "DELETE FROM " <> fmt table <> wh where
      wh = if null conditions then "" else "WHERE " <> commas conditions

instance FormatSql Setting where
    fmt (Setting column rhs) = fmt column <> "=" <> fmt rhs

instance FormatSql Update where
    fmt Update{table, settings, conditions} =
        "UPDATE " <> fmt table <> " SET " <> commas settings <> wh
      where wh = if null conditions then "" else "WHERE " <> commas conditions

instance FormatSql Select where
    fmt Select{table, columns, conditions} =
        "SELECT " <> commas columns <> " FROM " <> fmt table <> wh where
      wh = if null conditions then "" else "WHERE " <> commas conditions

instance FormatSql Condition where
    fmt (Op op column value) = fmt column <> " " <> fmt op <> " " <> fmt value

instance FormatSql Expr where
    fmt (Lit lit) = fmt lit
    fmt (Var name) = fmt name

instance FormatSql Op where
    fmt op = case op of
        Eq -> "="
        Syntax.LT -> "<"
        LTE -> "<="
        Syntax.GT -> ">"
        GTE -> ">="
        NEq -> "!="
