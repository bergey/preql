{-# LANGUAGE BlockArguments #-}
module Test.Syntax.RoundTrip where

import Preql.QuasiQuoter.Syntax.Parser
import Preql.QuasiQuoter.Syntax.Printer
import Preql.QuasiQuoter.Syntax.Syntax as Syntax
import Test.Syntax.Generators as Gen

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
    , knownCase (Simple Syntax.select
                 { from = [ J (Join Inner (Using [ "c" ]) (J (Table "a")) (J (Table "b"))) ] })
    ]
  , testGroup "hedgehog"
    [ roundTrip "literal" litE
    , roundTrip "Expr" expr
    , roundTrip "Select" Gen.select
    ]
  ]

testProperty :: TestName -> PropertyT IO () -> TestTree
testProperty testName = Tasty.testProperty testName . property

roundTrip :: (FormatSql sql, ParseSql sql, Show sql, Eq sql) =>
  TestName -> Gen sql -> TestTree
roundTrip testName gen = Tasty.testProperty testName $ property do
  ast <- forAll gen
  tripping ast formatAsString (parseSql testName)

knownCase :: (FormatSql sql, ParseSql sql, Show sql, Eq sql) => sql -> TestTree
knownCase e = testCase testName $
    assertEqual "knownCase" (Right e) (parseSql "knownCase" testName)
  where testName = formatAsString e

class ParseSql a where
  parseSql :: FilePath -> String -> Either String a

instance ParseSql Statement where parseSql = parseStatement
instance ParseSql SelectStmt where parseSql = parseSelect
instance ParseSql Expr where parseSql = parseExpr
