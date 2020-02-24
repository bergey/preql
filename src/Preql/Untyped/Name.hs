{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
-- | Definitions which need to be private in order to maintain their invariants.

module Preql.Untyped.Name (
    Name, mkName, getName
    ) where

import           Data.Data
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import           GHC.Generics
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift (..))

import qualified Data.Text                  as T

newtype Name = Name Text
    deriving (Show, Eq, Ord, Generic, Typeable, Data, Lift)

instance IsString Name where
    fromString = Name . T.pack

-- TODO mkName should enforce valid characters in Name
mkName :: Text -> Name
mkName = Name

getName :: Name -> Text
getName (Name name) = name
