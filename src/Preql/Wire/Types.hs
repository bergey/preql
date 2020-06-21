{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Preql.Wire.Types where

import Data.Text (Text)
import Data.Time (TimeOfDay, TimeZone)

data TimeTZ = TimeTZ !TimeOfDay !TimeZone
    deriving (Show, Eq)

newtype PgName = PgName Text
    deriving newtype (Show)
