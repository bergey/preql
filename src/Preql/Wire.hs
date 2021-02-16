{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | Description: Encode & Decode Postgres wire format
--
-- This module re-exports definitions from Wire.* that are expected to be useful

module Preql.Wire (
    -- * Decoding rows
    FromSql(..), FromSqlField
    -- * Encoding parameters
    , ToSql(..), ToSqlField
    -- * Errors
    , QueryError(..), FieldError(..), UnlocatedFieldError(..), TypeMismatch(..)
    , Tuple(..)
    , module X) where

import Preql.Wire.Errors as X
import Preql.Wire.Internal as X (Query, RowDecoder)
import Preql.Wire.ToSql as X
import Preql.Wire.Types as X
import Preql.Wire.Query as X (Connection, IsolationLevel(..), decodeVector)
import Preql.FromSql as X
