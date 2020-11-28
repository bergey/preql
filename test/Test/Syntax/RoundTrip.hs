{-# LANGUAGE BlockArguments #-}
module Test.Syntax.RoundTrip where

import Preql.QuasiQuoter.Syntax.Parser
import Preql.QuasiQuoter.Syntax.Printer
import Preql.QuasiQuoter.Syntax.Syntax
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
    , knownCase (L (LikeE Like (Lit (I 1))
                (L (LikeE Like (Unary IsNull (L (LikeE Like (Lit (I 2)) (Lit (I 3)) Nothing True)))
                  (Lit (I 4)) Nothing False))
                  Nothing False)
                )

    ]
  , testGroup "hedgehog"
    [ roundTrip "literal" litE
    , roundTrip "Expr" expr
    ]
  ]

testProperty :: TestName -> PropertyT IO () -> TestTree
testProperty testName = Tasty.testProperty testName . property

roundTrip :: TestName -> Gen Expr -> TestTree
roundTrip testName gen = Tasty.testProperty testName $ property do
  ast <- forAll gen
  tripping ast formatAsString (parseExpr testName)

knownCase :: Expr -> TestTree
knownCase e = testCase testName $
    assertEqual "knownCase" (Right e) (parseExpr "knownCase" testName)
  where testName = formatAsString e
