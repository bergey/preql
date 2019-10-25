module Instances where

import Internal
import Syntax

import Test.QuickCheck
import Generic.Random

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
    arbitrary = NE.fromList . getNonEmpty <$> arbitrary

instance Arbitrary Name where
    arbitrary = mkName . T.pack <$>
        (fmap getPrintableString arbitrary `suchThat`
            (\name -> not (any (`elem` " \t\n,.;'\"()<>=+-^!@") name)
                && (name /= "")
                && not (head name `elem` "0123456789_$")
            ))

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
instance Arbitrary Condition where arbitrary = genericArbitraryU
instance Arbitrary Expr where arbitrary = genericArbitraryU
instance Arbitrary BinOp where arbitrary = genericArbitraryU
instance Arbitrary UnaryOp where arbitrary = genericArbitraryU
instance Arbitrary Compare where arbitrary = genericArbitraryU
