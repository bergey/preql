module Preql.Wire.Types where

import           Data.Time (TimeOfDay, TimeZone)

data TimeTZ = TimeTZ !TimeOfDay !TimeZone
    deriving (Show, Eq)
