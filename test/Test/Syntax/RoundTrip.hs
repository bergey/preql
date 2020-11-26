{-# LANGUAGE BlockArguments #-}
module Test.Syntax.RoundTrip where

import Preql.QuasiQuoter.Syntax.Parser
import Preql.QuasiQuoter.Syntax.Printer
import Preql.QuasiQuoter.Syntax.Syntax (Expr(..), Indirection(..), Literal(..), UnaryOp(..))
import Test.Syntax.Generators

import Data.List.NonEmpty (NonEmpty(..))
import Hedgehog
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit
import qualified Test.Tasty.Hedgehog as Tasty (testProperty)

roundtrip :: TestTree
roundtrip = testGroup "roundtrip"
  [ testGroup "past failures"
    [ knownCase (Indirection (CRef "a") ("b" :| []))
    , knownCase (Indirection (CRef "a") ("b" :| ["c"]))
    , knownCase (Unary Negate (Lit (I 1)))
    , knownCase (Unary Negate (Lit (I 0)))
    ]
  , testGroup "hedgehog"
    [ roundTrip "literal" litE
    , roundTrip "Expr" expr
    ]
  ]

testProperty :: TestName -> PropertyT IO () -> TestTree
testProperty name = Tasty.testProperty name . property

roundTrip :: TestName -> Gen Expr -> TestTree
roundTrip name gen = Tasty.testProperty name $ property do
  ast <- forAll gen
  tripping ast formatAsString (parseExpr name)

knownCase :: Expr -> TestTree
knownCase e = testCase name $
    assertEqual "knownCase" (Right e) (normalizeExpr <$> parseExpr "knownCase" name)
  where name = formatAsString e
