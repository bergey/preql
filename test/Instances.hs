module Instances where

import           Untyped.Name
import           Untyped.Syntax

import           Control.Applicative
import           Data.Char           (toUpper)
import           Data.List.NonEmpty  (NonEmpty (..))
import           Generic.Random
import           Test.QuickCheck

import qualified Data.Text           as T

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = liftA2 (:|) arbitrary arbitrary

keywords = ["DELETE", "SELECT", "INSERT", "FROM", "WHERE", "INTO", "VALUES", "IS", "NOT", "NULL", "ISNULL", "NOTNULL", "LIKE", "ILIKE", "AND", "OR" ]

-- | These ASCII letters are only a small subset of allowed identifiers.
instance Arbitrary Name where
    -- listOf uses size; it's good to limit name length, incidental that it gets smaller deeper in the AST
    -- another sensible option would be to resize to 32, the Postgres effective name limit
    arbitrary = mkName . T.pack <$> (liftA2 (:) firstChar (listOf tailChar)
        `suchThat` (\name -> not (map toUpper name `elem` keywords)))
      where
        firstChar = elements $ ['a'..'z'] ++ ['A'..'Z']
        tailChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "0123456789_$"

-- instance Arbitrary Literal where arbitrary = genericArbitraryU
instance Arbitrary Literal where
    -- TODO remaining Literal constructors
    arbitrary = oneof
        [ F <$> arbitrary
        , T . T.pack <$> fmap getPrintableString arbitrary `suchThat` (all (/= '\''))
        ]

instance Arbitrary Query where arbitrary = genericArbitraryU
instance Arbitrary Insert where arbitrary = genericArbitraryU
instance Arbitrary Delete where arbitrary = genericArbitraryU
instance Arbitrary Setting where arbitrary = genericArbitraryU
instance Arbitrary Update where arbitrary = genericArbitraryU
instance Arbitrary OldSelect where arbitrary = genericArbitraryU

-- Recursive types need more careful treatment to avoid infinite trees
instance Arbitrary Condition where
    arbitrary = do
        size <- getSize
        if size <= 1
            then Compare <$> arbitrary <*> arbitrary <*> arbitrary
            else scale  (`div` 2) genericArbitraryU
    shrink = genericShrink

instance Arbitrary Expr where
    arbitrary = do
        size <- getSize
        if size <= 1
            then oneof [ Lit <$> arbitrary, Var <$> arbitrary ]
            else scale (`div` 2) $ oneof
                 [ Lit <$> arbitrary, Var <$> arbitrary
                 , NumberedParam <$> arbitrary <*> pure [] -- never InlineParam
                 , BinOp <$> arbitrary <*> arbitrary <*> arbitrary
                 , Unary <$> arbitrary <*> arbitrary
                 ]
    shrink expr = case expr of
        -- either subterm, or shrink one or both subterms
        BinOp op l r -> l : r : (tail $ BinOp op <$> l : shrink l <*> r : shrink r)
        Unary op e -> e : (Unary op <$> shrink e)
        _ -> [] -- remaning terms can't be shrunk

instance Arbitrary BinOp where arbitrary = genericArbitraryU
instance Arbitrary UnaryOp where arbitrary = genericArbitraryU
instance Arbitrary Compare where arbitrary = genericArbitraryU
