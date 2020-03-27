{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances

module Preql.Wire.Orphans where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), withScientific)
import Data.Scientific (toBoundedInteger)
import qualified Database.PostgreSQL.LibPQ as PQ

-- TODO use a locally-defined PgType, and hang instances on that, instead

instance ToJSON PQ.Oid where
    toJSON (PQ.Oid oid) = Number (fromIntegral oid)

instance FromJSON PQ.Oid where
    parseJSON = withScientific "Oid" $ \sci ->
        case toBoundedInteger sci of
            Just i -> return (PQ.Oid i)
            Nothing -> fail "expected integer Oid"
