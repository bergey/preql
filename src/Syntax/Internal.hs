{-# LANGUAGE DeriveGeneric #-}
-- | Definitions which need to be private in order to maintain their invariants.

module Syntax.Internal (
    Name, mkName, getName
    ) where

import           Data.String (IsString (..))
import           Data.Text (Text)
import           GHC.Generics

import qualified Data.Text as T

newtype Name = Name Text
    deriving (Show, Eq, Ord, Generic)

instance IsString Name where
    fromString = Name . T.pack

-- TODO mkName should enforce valid characters in Name
mkName :: Text -> Name
mkName = Name

getName :: Name -> Text
getName (Name name) = name
