{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Data types reperesenting SQL query syntax.

module Untyped.Syntax where

import           Untyped.Name

import           Data.Data
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Text                  (Text)
import           GHC.Generics
import           Instances.TH.Lift
import           Language.Haskell.TH.Syntax (Lift (..))

import qualified Data.Text                  as T

data Literal = I !Int | F !Double | T !Text | B !Bool
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Query = QI !Insert | QD !Delete | QU !Update | QS !Select
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

-- | Queries of the form @INSERT INTO table (columns) VALUES (values);@
-- Limitations:
-- * single row
-- * no @ON CONFLICT@
data Insert = Insert
    { table   :: !Name
    , columns :: NonEmpty Name
    , values  :: NonEmpty Expr -- TODO enforce matched lengths?
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

-- | Queries of the form @DELETE FROM table WHERE conditions@.
data Delete = Delete
    { table      :: !Name
    , conditions :: Maybe Condition
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Setting = Setting !Name !Expr
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

-- | Queries of the form @UPDATE table SET settings WHERE conditions@.  Where each
-- @Setting name literal@ is like SQL @name = literal@.
data Update = Update
    { table      :: !Name
    , settings   :: NonEmpty Setting
    , conditions :: Maybe Condition
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

-- | Queries of the form @SELECT columns FROM table WHERE conditions@.
data Select = Select
    { table      :: !Name
    , columns    :: NonEmpty Expr
    , conditions :: Maybe Condition
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Condition = Compare !Compare !Name !Expr
    | Or Condition Condition
    | And Condition Condition
    | Not Condition
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Expr = Lit !Literal | Var !Name
    | NumberedParam !Word | InlineParam !Text
    | BinOp !BinOp !Expr !Expr
    | Unary !UnaryOp !Expr
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data BinOp = Mul | Div | Add | Sub | Exponent | Comp !Compare
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data UnaryOp = NegateNum | NegateBool | IsNull | NotNull
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Compare = Eq | LT | LTE | GT | GTE | NEq |  Like | ILike
    deriving (Show, Eq, Generic, Typeable, Data, Lift)
