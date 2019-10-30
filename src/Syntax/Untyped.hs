{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Data types reperesenting SQL query syntax.

module Syntax.Untyped where

import           Syntax.Internal

import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           GHC.Generics

import qualified Data.Text as T

data Literal = I !Int | F !Double | T !Text | B !Bool
    deriving (Show, Eq, Generic)

data Query = QI !Insert | QD !Delete | QU !Update | QS !Select
    deriving (Show, Eq, Generic)

-- | Queries of the form @INSERT INTO table (columns) VALUES (values);@
-- Limitations:
-- * single row
-- * no @ON CONFLICT@
data Insert = Insert
    { table   :: !Name
    , columns :: NonEmpty Name
    , values  :: NonEmpty Literal -- TODO enforce matched lengths?
    } deriving (Show, Eq, Generic)

-- | Queries of the form @DELETE FROM table WHERE conditions@.
data Delete = Delete
    { table      :: !Name
    , conditions :: Maybe Condition
    } deriving (Show, Eq, Generic)

-- TODO Expressions besides Literals
data Setting = Setting !Name !Literal
    deriving (Show, Eq, Generic)

-- | Queries of the form @UPDATE table SET settings WHERE conditions@.  Where each
-- @Setting name literal@ is like SQL @name = literal@.
data Update = Update
    { table      :: !Name
    , settings   :: NonEmpty Setting
    , conditions :: Maybe Condition
    } deriving (Show, Eq, Generic)

-- | Queries of the form @SELECT columns FROM table WHERE conditions@.
data Select = Select
    { table      :: !Name
    , columns    :: NonEmpty Name
    , conditions :: Maybe Condition
    } deriving (Show, Eq, Generic)

data Condition = Compare !Compare !Name !Expr
    | Or Condition Condition
    | And Condition Condition
    | Not Condition
    deriving (Show, Eq, Generic)

data Expr = Lit !Literal | Var !Name
    | BinOp !BinOp !Expr !Expr
    | Unary !UnaryOp !Expr
    deriving (Show, Eq, Generic)

data BinOp = Mul | Div | Add | Sub | Exponent | Comp !Compare
    deriving (Show, Eq, Generic)

data UnaryOp = NegateNum | NegateBool | IsNull | NotNull
    deriving (Show, Eq, Generic)

data Compare = Eq | LT | LTE | GT | GTE | NEq |  Like | ILike
    deriving (Show, Eq, Generic)
