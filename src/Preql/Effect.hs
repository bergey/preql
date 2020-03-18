-- | SQL Effect class, basically capturing some way of accessing a database.

module Preql.Effect
    ( Query, SQL(..), Transaction, runTransactionIO
    ) where

import Preql.Effect.Internal
import Preql.Wire.Internal
