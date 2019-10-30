module Instances where

import           Syntax.Internal
import           Syntax.Untyped

import           Control.Applicative
import           Data.List.NonEmpty (NonEmpty(..))
import           Generic.Random
import           Test.QuickCheck

import qualified Data.Text as T

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = liftA2 (:|) arbitrary arbitrary

-- | These ASCII letters are only a small subset of allowed identifiers.
instance Arbitrary Name where
    -- listOf uses size; it's good to limit name length, incidental that it gets smaller deeper in the AST
    -- another sensible option would be to resize to 32, the Postgres effective name limit
    arbitrary = mkName . T.pack <$> liftA2 (:) firstChar (listOf tailChar) where
      firstChar = elements $ ['a'..'z'] ++ ['A'..'Z']
      tailChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "0123456789_$"

-- instance Arbitrary Literal where arbitrary = genericArbitraryU
instance Arbitrary Literal where
    -- TODO remaining Literal constructors
    arbitrary = oneof
        [ F <$> arbitrary
        , T . T.pack . getPrintableString <$> arbitrary
        ]

instance Arbitrary Query where arbitrary = genericArbitraryU
instance Arbitrary Insert where arbitrary = genericArbitraryU
instance Arbitrary Delete where arbitrary = genericArbitraryU
instance Arbitrary Setting where arbitrary = genericArbitraryU
instance Arbitrary Update where arbitrary = genericArbitraryU
instance Arbitrary Select where arbitrary = genericArbitraryU

-- Recursive types need more careful treatment to avoid infinite trees
instance Arbitrary Condition where
    arbitrary = do
        size <- getSize
        if size <= 1
            then Compare <$> arbitrary <*> arbitrary <*> arbitrary
            else scale (\s -> s - 1) genericArbitraryU

instance Arbitrary Expr where
    arbitrary = do
        size <- getSize
        if size <= 1
            then oneof [ Lit <$> arbitrary, Var <$> arbitrary ]
            else scale (\s -> s - 1) genericArbitraryU
        genericArbitraryU

instance Arbitrary BinOp where arbitrary = genericArbitraryU
instance Arbitrary UnaryOp where arbitrary = genericArbitraryU
instance Arbitrary Compare where arbitrary = genericArbitraryU
