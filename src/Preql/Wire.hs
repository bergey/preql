{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module re-exports definitions from Wire.* that are expected to be useful

module Preql.Wire (
    -- * Decoding rows
    FromSql, FromSqlField
    -- * Encoding parameters
    , ToSql, ToSqlField
    -- * Errors
    , QueryError(..), FieldError(..), UnlocatedFieldError(..), TypeMismatch(..)
    , module X) where

import Preql.Wire.Decode as X
import Preql.Wire.Errors as X
import Preql.Wire.Internal as X (Query, RowDecoder)
import Preql.Wire.ToSql as X
import Preql.Wire.Types as X
import Preql.Wire.Query as X (IsolationLevel(..))
import Preql.FromSql.Class as X
