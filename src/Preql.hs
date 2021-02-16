{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | Description: Import this

module Preql (
    SQL(..), SqlQuery(..), sql, select, validSql
    , Transaction, Query
    -- * functions for writing SQL instances
    , runTransactionIO
    -- * Decoding rows
    , FromSql, FromSqlField
    -- * Encoding parameters
    , ToSql, ToSqlField
    -- * Errors
    , QueryError(..), FieldError(..), UnlocatedFieldError(..), TypeMismatch(..)
    -- | encoding & decoding to wire format
    , module Preql.Wire
    ) where

import           Preql.Effect
import           Preql.QuasiQuoter.Raw.TH    (sql)
import           Preql.QuasiQuoter.Syntax.TH (select, validSql)
import           Preql.Wire
