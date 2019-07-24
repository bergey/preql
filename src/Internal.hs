-- | Definitions which need to be private in order to maintain their invariants.

module Internal (
    Name, mkName, getName
    ) where

import           Data.Text (Text)

newtype Name = Name Text
    deriving (Show, Eq, Ord)

-- TODO mkName should enforce valid characters in Name
mkName :: Text -> Name
mkName = Name

getName :: Name -> Text
getName (Name name) = name
