{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Description: Syntax tree for SQL

module Preql.QuasiQuoter.Syntax.Syntax where

import Preql.QuasiQuoter.Syntax.Name

import Data.Data
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Word (Word)
import GHC.Generics
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift(..))
import qualified Data.Text as T

-- FIXME rename to Constant?
data Literal = I !Word | F !Double | T !Text | B !Bool | Null
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Statement = QI !Insert | QD !Delete | QU !Update | QS !SelectStmt
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
    , conditions :: Maybe Expr
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Setting = Setting !Name !Expr
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

-- | Queries of the form @UPDATE table SET settings WHERE conditions@.  Where each
-- @Setting name literal@ is like SQL @name = literal@.
data Update = Update
    { table      :: !Name
    , settings   :: NonEmpty Setting
    , conditions :: Maybe Expr
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

-- TODO prevent multiple SelectOptions on the same query
-- If each constructor takes SelectOptions, we can ditch S and the empty SelectOptions becomes valid
data SelectStmt
    = SelectValues (NonEmpty (NonEmpty Expr))
    | Simple Select
    | S SelectStmt SelectOptions
    | Set SetOp AllOrDistinct SelectStmt SelectStmt
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Select = Select
    { distinct :: Maybe DistinctClause
    , targetList :: [ResTarget]
    , from :: [TableRef]
    , whereClause :: Maybe Expr
    , groupBy :: [Expr] -- TODO more accurate type than Expr?
    , having :: Maybe Expr
    , window :: [Window]
    -- TODO remaining fields
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

data SelectOptions = SelectOptions
    { sortBy :: [SortBy]
    , offset :: Maybe Expr
    , limit :: Maybe Expr
    , locking :: [Locking]
    , withClause :: Maybe WithClause
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

-- This is really for writing tests, but put it here for faster type check errors
select :: Select
select = Select
    { distinct = Nothing
    , targetList = []
    , from = []
    , whereClause = Nothing
    , groupBy = []
    , having = Nothing
    , window = []
    }

selectOptions :: SelectOptions
selectOptions = SelectOptions
    { sortBy = []
    , offset = Nothing
    , limit = Nothing
    , locking = []
    , withClause = Nothing
    }

data TableRef
  = J JoinedTable
  | As JoinedTable Alias
  | SubSelect SelectStmt Alias
  deriving (Show, Eq, Generic, Typeable, Data, Lift)

data JoinedTable
  = Table Name
  | Join JoinType JoinQual TableRef TableRef
  | CrossJoin TableRef TableRef
  deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Alias = Alias
    { aliasName :: Name
    , columnNames :: [ Name ]
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

data JoinType = Inner | LeftJoin | RightJoin | Full
    deriving (Show, Eq, Generic, Typeable, Data, Lift, Enum, Bounded)

data JoinQual = Using [Name] | On Expr | Natural
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data DistinctClause = DistinctAll | DistinctOn (NonEmpty Expr)
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data SetOp = Union | Intersect | Except
    deriving (Show, Eq, Generic, Typeable, Data, Lift, Enum, Bounded)

data AllOrDistinct = All | Distinct
    deriving (Show, Eq, Generic, Typeable, Data, Lift, Enum, Bounded)

data ResTarget = Star | Column Expr (Maybe Name)
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Window = Window
    { name :: Maybe Name
    , refName :: Maybe Name
    , partitionClause :: [Expr]
    , orderClause :: [SortBy ]
    -- , frameOptions :: _ -- FIXME implement
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

data SortBy = SortBy
    { column :: Expr
    , direction :: SortOrderOrUsing
    , nulls :: NullsOrder
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

data SortOrderOrUsing = SortOrder SortOrder | SortUsing BinOp
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data SortOrder = Ascending | Descending | DefaultSortOrder
    deriving (Show, Eq, Generic, Typeable, Data, Lift, Enum, Bounded)

data NullsOrder = NullsFirst | NullsLast | NullsOrderDefault
    deriving (Show, Eq, Generic, Typeable, Data, Lift, Enum, Bounded)

data Locking = Locking
    { strength :: LockingStrength
    , tables :: [Name]
    , wait :: LockWait
    } deriving (Show, Eq, Generic, Data, Lift)

data LockingStrength
    = ForUpdate | ForNoKeyUpdate | ForShare | ForKeyShare
    deriving (Show, Eq, Enum, Bounded, Data, Lift, Generic)

data LockWait = LockWaitError | LockWaitSkip | LockWaitBlock
    deriving (Show, Eq, Enum, Bounded, Data, Lift, Generic)

data WithClause = With
  { commonTables :: [ CTE ]
  , recursive :: Recursive
  }
  deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Recursive = Recursive | NotRecursive
    deriving (Show, Eq, Enum, Bounded, Data, Lift, Generic)

data Materialized = Materialized | NotMaterialized | MaterializeDefault
    deriving (Show, Eq, Enum, Bounded, Data, Lift, Generic)

data CTE = CommonTableExpr
  { name :: Name
  , aliases :: [Name]
  , materialized :: Materialized
  , query :: Statement
  }
  deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Expr = Lit !Literal | CRef Name
    | NumberedParam !Word
    | HaskellParam !Text
    | BinOp !BinOp !Expr !Expr
    | Unary !UnaryOp !Expr
    | Indirection Expr (NonEmpty Indirection)
    | SelectExpr SelectStmt
    | L LikeE
    | Fun FunctionApplication
    | Cas Case
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

type Indirection = Name -- FIXME

data BinOp = Mul | Div | Add | Sub | Exponent | Mod
           | Eq | LT | LTE | GT | GTE | NEq
           | IsDistinctFrom | IsNotDistinctFrom
           | And | Or
    deriving (Show, Eq, Generic, Typeable, Data, Lift, Bounded, Enum)

data UnaryOp = Negate | Not | IsNull | NotNull
    deriving (Show, Eq, Generic, Typeable, Data, Lift, Bounded, Enum)

data LikeOp = Like | ILike | Similar -- TODO add ~ !~ ~* !~*
    deriving (Show, Eq, Generic, Typeable, Data, Lift, Bounded, Enum)

data LikeE = LikeE
    { op :: LikeOp
    , string :: Expr
    , likePattern :: Expr
    , escape :: Maybe Expr
    , invert :: Bool
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

like :: LikeOp -> Expr -> Expr -> LikeE
like op string likePattern =
    LikeE { op, string, likePattern, escape = Nothing, invert = False }

data FunctionApplication = FApp
    { name :: Name
    , indirection :: [Indirection]
    , arguments :: FunctionArguments
    , sortBy :: [SortBy]
    , distinct :: Bool
    , withinGroup :: Bool
    , filterClause :: Maybe Expr
    , over :: Maybe Window
    } deriving (Show, Eq, Generic, Typeable, Data, Lift)

fapp :: (Name, [Indirection]) -> [Argument] -> FunctionApplication
fapp (name, indirection) args = FApp
    { name, indirection
    , arguments = A args
    , sortBy = []
    , distinct = False
    , withinGroup = False
    , filterClause = Nothing
    , over = Nothing
    }

fapp1 :: Name -> [Expr] -> FunctionApplication
fapp1 fName args = fapp (fName, []) (map E args)

data FunctionArguments = StarArg | A [Argument]
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Argument = E Expr | Named Name Expr
    deriving (Show, Eq, Generic, Typeable, Data, Lift)

data Case = Case
  { whenClause :: [(Expr, Expr)] -- (condition, then)
  , implicitArg :: Maybe Expr
  , elseClause :: Maybe Expr
  } deriving (Show, Eq, Generic, Data, Lift)
