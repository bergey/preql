{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Instances where

import           Preql.QuasiQuoter.Syntax.Name
import           Preql.QuasiQuoter.Syntax.Syntax

import           Control.Applicative
import           Data.Char           (toUpper)
import           Data.List.NonEmpty  (NonEmpty (..))
import           Generic.Random
import           Test.QuickCheck

import qualified Data.Text           as T

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = liftA2 (:|) arbitrary arbitrary

keywords :: [String]
keywords = ["DELETE", "SELECT", "INSERT", "FROM", "WHERE", "INTO", "VALUES", "IS", "NOT", "NULL", "ISNULL", "NOTNULL", "LIKE", "ILIKE", "AND", "OR" ]

-- | These ASCII letters are only a small subset of allowed identifiers.
instance Arbitrary Name where
    -- listOf uses size; it's good to limit name length, incidental that it gets smaller deeper in the AST
    -- another sensible option would be to resize to 32, the Postgres effective name limit
    arbitrary = mkName . T.pack <$> (liftA2 (:) firstChar (listOf tailChar)
        `suchThat` (\n -> map toUpper n `notElem` keywords))
      where
        firstChar = elements $ ['a'..'z'] ++ ['A'..'Z']
        tailChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "0123456789_$"

-- instance Arbitrary Literal where arbitrary = genericArbitraryU
instance Arbitrary Literal where
    -- TODO remaining Literal constructors
    arbitrary = oneof
        [ F <$> arbitrary
        , T . T.pack <$> fmap getPrintableString arbitrary `suchThat` notElem '\''
        , B <$> arbitrary
        , pure Null
        ]

instance Arbitrary Statement where arbitrary = genericArbitraryU
instance Arbitrary Insert where arbitrary = genericArbitraryU
instance Arbitrary Delete where arbitrary = genericArbitraryU
instance Arbitrary Setting where arbitrary = genericArbitraryU
instance Arbitrary Update where arbitrary = genericArbitraryU
instance Arbitrary SelectStmt where arbitrary = genericArbitraryU
instance Arbitrary Select where arbitrary = genericArbitraryU
instance Arbitrary SelectOptions where arbitrary = genericArbitraryU
instance Arbitrary TableRef where arbitrary = genericArbitraryU
instance Arbitrary Alias where arbitrary = genericArbitraryU
instance Arbitrary JoinType where arbitrary = genericArbitraryU
instance Arbitrary JoinQual where arbitrary = genericArbitraryU
instance Arbitrary DistinctClause where arbitrary = genericArbitraryU
instance Arbitrary SetOp where arbitrary = genericArbitraryU
instance Arbitrary AllOrDistinct where arbitrary = genericArbitraryU
instance Arbitrary ResTarget where arbitrary = genericArbitraryU
-- instance Arbitrary ColumnRef where arbitrary = genericArbitraryU
instance Arbitrary Window where arbitrary = genericArbitraryU
instance Arbitrary SortBy where arbitrary = genericArbitraryU
instance Arbitrary SortOrderOrUsing where arbitrary = genericArbitraryU
instance Arbitrary SortOrder where arbitrary = genericArbitraryU -- TODO better for enum?
instance Arbitrary NullsOrder where arbitrary = genericArbitraryU
instance Arbitrary Locking where arbitrary = genericArbitraryU
instance Arbitrary LockingStrength where arbitrary = genericArbitraryU
instance Arbitrary LockWait where arbitrary = genericArbitraryU
instance Arbitrary WithClause where arbitrary = genericArbitraryU
instance Arbitrary Recursive where arbitrary = genericArbitraryU
instance Arbitrary Materialized where arbitrary = genericArbitraryU
instance Arbitrary CTE where arbitrary = genericArbitraryU


-- Recursive types need more careful treatment to avoid infinite trees
instance Arbitrary Expr where
    arbitrary = do
        size <- getSize
        if size <= 1
            then oneof [ Lit <$> arbitrary, CRef <$> arbitrary ]
            else oneof (scale (`div` 2) <$>
                        [ BinOp <$> arbitrary <*> arbitrary <*> arbitrary
                        , And <$> arbitrary <*> arbitrary
                        , Or <$> arbitrary <*> arbitrary
                        , Indirection <$> arbitrary <*> arbitrary
                        , SelectExpr <$> arbitrary <*> arbitrary
                        ]
                        <> map (scale pred)
                        [ Lit <$> arbitrary, CRef <$> arbitrary
                        , NumberedParam <$> arbitrary <*> pure []
                        , Unary <$> arbitrary <*> arbitrary
                        , Not <$> arbitrary
                        ]
                       )
    shrink expr = case expr of
        -- either subterm, or shrink one or both subterms
        BinOp op l r -> l : r : tail (BinOp op <$> l : shrink l <*> r : shrink r)
        Unary op e -> e : (Unary op <$> shrink e)
        _ -> [] -- remaning terms can't be shrunk

instance Arbitrary ColumnRef where arbitrary = genericArbitraryU
instance Arbitrary BinOp where arbitrary = genericArbitraryU
instance Arbitrary UnaryOp where arbitrary = genericArbitraryU
