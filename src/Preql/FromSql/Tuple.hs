-- | Description: newtype to support FromSql instances for row types.

module Preql.FromSql.Tuple where

-- | Wrapper for Postgres anonymous row types (sometimes called record
-- types), so instance resolution picks the right decoder.  The useful
-- instances are for (Haskell) tuples.  Postgres allows row types with
-- a single field, but the instances would overlap with those for
-- nested row types, so we do not provide them.
newtype Tuple r = Tuple r
  deriving (Eq, Ord, Show)
