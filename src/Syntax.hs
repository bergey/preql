{-# LANGUAGE DuplicateRecordFields #-}

-- | Data types reperesenting SQL query syntax.

module Syntax where

import           Internal

import           Data.List.NonEmpty (NonEmpty)
import           Data.Text          (Text)

import qualified Data.Text          as T

data Literal = I !Int | F !Double | T !Text | B !Bool
    deriving (Show, Eq)

data Query = QI !Insert | QD !Delete | QU !Update | QS !Select
    deriving (Show, Eq)

-- | Queries of the form @INSERT INTO table (columns) VALUES (values);@
-- Limitations:
-- * single row
-- * no @ON CONFLICT@
data Insert = Insert
    { table   :: !Name
    , columns :: NonEmpty Name
    , values  :: NonEmpty Literal -- TODO enforce matched lengths?
    } deriving (Show, Eq)

-- | Queries of the form @DELETE FROM table WHERE conditions@.
data Delete = Delete
    { table      :: !Name
    , conditions :: Maybe Condition
    } deriving (Show, Eq)

-- TODO Expressions besides Literals
data Setting = Setting !Name !Literal
    deriving (Show, Eq)

-- | Queries of the form @UPDATE table SET settings WHERE conditions@.  Where each
-- @Setting name literal@ is like SQL @name = literal@.
data Update = Update
    { table      :: !Name
    , settings   :: NonEmpty Setting
    , conditions :: Maybe Condition
    } deriving (Show, Eq)

-- | Queries of the form @SELECT columns FROM table WHERE conditions@.
data Select = Select
    { table      :: !Name
    , columns    :: NonEmpty Name
    , conditions :: Maybe Condition
    } deriving (Show, Eq)

data Condition = Op !Op !Name !Expr
    | Or Condition Condition
    | And Condition Condition
    deriving (Show, Eq)

-- TODO many more types of expressions
data Expr = Lit !Literal | Var !Name
    deriving (Show, Eq)

data Op = Eq | LT | LTE | GT | GTE | NEq
    deriving (Show, Eq)
