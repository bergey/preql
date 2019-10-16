-- | For now, re-export Connection code from postgresql-simple

module Connection
    ( Connection(..), ConnectInfo(..), defaultConnectInfo
    , connect, connectPostgreSQL
    ) where

import           Database.PostgreSQL.Simple.Internal
