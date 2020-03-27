{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Preql (
    SQL(..), sql
    , Transaction, Query
    -- * functions for writing SQL instances
    , runTransactionIO
    -- | encoding & decoding to wire format
    , module Preql.Wire
    ) where

import Preql.Wire
import Preql.QuasiQuoter.Raw.TH (sql)
import Preql.Effect
