{-# LANGUAGE DuplicateRecordFields #-}

-- | Data types reperesenting SQL query syntax.

module Syntax where

import           Internal

import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)

import qualified Data.Text as T

data Literal = I !Int | F !Double | T !Text | B !Bool
    deriving (Show, Eq)

data Query = QI !Insert | QD !Delete | QU !Update | QS !Select
    deriving (Show, Eq)

-- | Queries of the form @INSERT INTO table (columns) VALUES (values);@
-- Limitations:
-- * single row
-- * no @ON CONFLICT@
data Insert = Insert
    { table :: !Name
    , columns :: NonEmpty Name
    , values :: NonEmpty Literal -- TODO enforce matched lengths?
    } deriving (Show, Eq)

-- TODO conditions
data Delete = Delete
    { table :: !Name
    } deriving (Show, Eq)

-- TODO Expressions besides Literals
data Setting = Setting !Name !Literal
    deriving (Show, Eq)

-- | Queries of the form @UPDATE table SET settings@.  Where each
-- @Setting name literal@ is like SQL @name = literal@.
data Update = Update
    { table :: !Name
    , settings :: NonEmpty Setting
    } deriving (Show, Eq)

-- | Queries of the form @SELECT columns FROM table@.
data Select = Select
    { table :: !Name
    , columns :: NonEmpty Name
    -- TODO conditions
    } deriving (Show, Eq)
