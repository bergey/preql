{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Data types reperesenting SQL query syntax.

module Preql.QuasiQuoter.Syntax.Syntax where

import           Preql.QuasiQuoter.Syntax.Name

import           Data.Data
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Text                  (Text)
import           GHC.Generics
import           Instances.TH.Lift
import           Language.Haskell.TH.Syntax (Lift (..))

import qualified Data.Text                  as T

-- FIXME rename to Constant?
data Literal = I !Int | F !Double | T !Text | B !Bool | Null
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Query = QI !Insert | QD !Delete | QU !Update | QS !OldSelect
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
data OldSelect = OldSelect
    { table      :: !Name
    , columns    :: NonEmpty Expr
    , conditions :: Maybe Condition
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

data SelectStmt
    = SelectValues (NonEmpty (NonEmpty Expr))
    | SelectUnordered Unordered
    | SortedSelect SelectStmt (NonEmpty SortBy)
    -- TODO more cases
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Unordered = Unordered
    { distinct :: Maybe DistinctClause
    , targetList :: [ResTarget]
    , from :: [TableRef]
    , whereClause :: Maybe Expr
    , groupBy :: [Expr] -- TODO more accurate type than Expr?
    , having :: Maybe Expr
    , window :: [Window]
    -- TODO remaining fields
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

-- TODO joins
data TableRef = TableRef
    { relation :: Name -- TODO
    , alias :: Maybe Alias
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Alias = Alias
    { aliasName :: Name
    , columnNames :: [ Name ]
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

data SortBy = SortBy
    { column :: Expr
    , direction :: SortOrderOrUsing
    , nulls :: NullsOrder
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

data SortOrderOrUsing = SortOrder SortOrder | Using BinOp
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data SortOrder = Ascending | Descending | DefaultSortOrder
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data NullsOrder = NullsFirst | NullsLast | NullsOrderDefault
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Condition = Compare !Compare !Name !Expr
    | Or Condition Condition
    | And Condition Condition
    | Not Condition
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Expr = Lit !Literal | Var !Name
    | NumberedParam !Word [Indirection]
    | HaskellParam !Text
    | BinOp !BinOp !Expr !Expr
    | Unary !UnaryOp !Expr
    | CRef ColumnRef
    | Indirection Expr [Indirection]
    | SelectExpr SelectStmt [Indirection]
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

type Indirection = Name -- FIXME

data BinOp = Mul | Div | Add | Sub | Exponent | Mod | Comp !Compare
           | IsDistinctFrom | IsNotDistinctFrom
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data UnaryOp = NegateNum | NegateBool | IsNull | NotNull
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Compare = Eq | LT | LTE | GT | GTE | NEq |  Like | ILike
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data AllOrDistinct = All | Distinct
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data DistinctClause = DistinctAll | DistinctOn (NonEmpty Expr)
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data ResTarget = Star | ColumnTarget ColumnRef
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data ColumnRef = ColumnRef
    { value :: Expr
    , name :: Maybe Name -- TODO bigger type
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Window = Window
    { name :: Maybe Name
    , refName :: Maybe Name
    , partitionClause :: [Expr]
    , orderClause :: [SortBy ]
    , frameOptions :: () -- FIXME implement
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)
