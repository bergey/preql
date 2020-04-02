{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

import Test.Wire (wire)

import Preql.QuasiQuoter.Raw.TH as Raw
import Untyped.Name
import Untyped.Params as Syntax (AntiquoteState(..), numberAntiquotes)
import Untyped.Parser
import Untyped.Printer
import Untyped.Syntax
import qualified Untyped.Lex as L

import Data.Either
import Data.List.NonEmpty (NonEmpty(..))
import Prelude hiding (Ordering(..), lex)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Preql.QuasiQuoter.Raw.Lex as Raw

main :: IO ()
main = defaultMain $ testGroup "crispy-broccoli"
    [ antiquotes
    , wire
    -- , integration
    ]

-- | Tests of the SQL syntax printer
printer :: TestTree
printer = testGroup "printer" [
    testCase "DELETE, no condition" $
        assertEqual ""
            "DELETE FROM taffy"
            (fmt (QD Delete
                  { table = mkName "taffy"
                  , conditions = Nothing
                  }))
    , testCase "DELETE, = condition" $
        assertEqual ""
            "DELETE FROM taffy WHERE flavor = 'blueberry'"
            (fmt (QD Delete
                  { table = mkName "taffy"
                  , conditions = Just (Compare Eq (mkName "flavor") (Lit (T"blueberry")))
                  }))
    , testCase "INSERT, one column" $
        assertEqual ""
            "INSERT INTO users (email) VALUES ('bergey@teallabs.org')"
            (fmt (QI Insert
                 { table = mkName "users"
                 , columns = mkName "email" :| []
                 , values = Lit (T "bergey@teallabs.org") :| []
                 }))
    , testCase "INSERT, two columns" $
        assertEqual ""
            "INSERT INTO users (email, first_name) VALUES ('bergey@teallabs.org', 'Daniel')"
            (fmt (QI Insert
                 { table = mkName "users"
                 , columns = mkName "email" :| [ mkName "first_name" ]
                 , values = Lit (T "bergey@teallabs.org") :| [ Lit (T "Daniel") ]
                 }))
    , testCase "params" $
      assertEqual ""
        "SELECT name, email FROM users WHERE name = $1"
        (fmt(QS OldSelect
              { table = "users"
              , columns = Var "name" :| [ Var "email"]
              , conditions = Just (Compare Eq "name" (NumberedParam 1 []))
              }))
    ]

parser :: TestTree
parser = testGroup "parser"
    [ testParse "DELETE FROM taffy"
        (QD (Delete (mkName "taffy") Nothing))
    , testParse "dEleTe FROM taffy WHERE flavor = 'blueberry'"
      (QD Delete
          { table = mkName "taffy"
          , conditions = Just (Compare Eq (mkName "flavor") (Lit (T"blueberry")))
          })
    , testParse "DELETE FROM users WHERE email != 'bergey@teallabs.org'"
      (QD Delete
       { table = mkName "users"
       , conditions = Just (Compare NEq (mkName "email") (Lit (T "bergey@teallabs.org")))
       })
    , testParse "INSERT INTO users (email) VALUES ('bergey@teallabs.org')"
        (QI Insert
            { table = mkName "users"
            , columns = mkName "email" :| []
            , values = Lit (T "bergey@teallabs.org") :| []
            })
    , testParse "INSERT INTO addresses (street, country) VALUES ('4 Ames St', 'USA')"
      (QI Insert
       { table = "addresses"
       , columns = "street" :| ["country" ]
       , values = Lit (T "4 Ames St") :| [ Lit (T "USA") ]
       })
    , testParse "SELECT name FROM users"
      (QS OldSelect
       { table = "users"
       , columns = CRef (ColumnRef (Var "name") Nothing) :| []
       , conditions = Nothing
       })
    , testParse "SELECT name, email FROM users"
      (QS OldSelect
       { table = "users"
       , columns = CRef (ColumnRef (Var "name") Nothing) :| [CRef (ColumnRef (Var "email") Nothing)]
       , conditions = Nothing
       })
    , testParse "SELECT name, email FROM users WHERE name = 'Daniel'"
      (QS OldSelect
       { table = "users"
       , columns = CRef (ColumnRef (Var "name") Nothing) :| [CRef (ColumnRef (Var "email") Nothing)]
       , conditions = Just (Compare Eq "name" (Lit (T "Daniel")))
       })
    , testParse "SELECT name, email FROM users WHERE name = 'Daniel' OR name = 'Bergey'"
      (QS OldSelect
       { table = "users"
       , columns = CRef (ColumnRef (Var "name") Nothing) :| [CRef (ColumnRef (Var "email") Nothing)]
       , conditions = Just (Or (Compare Eq "name" (Lit (T "Daniel"))) (Compare Eq "name" (Lit (T "Bergey"))))
       })
    , testParse "SELECT name FROM users WHERE age = 35"
        -- We currently parse integers & decimals all to Double
        -- Just test that both parser rules work
      (QS OldSelect
       { table = "users"
       , columns = CRef (ColumnRef (Var "name") Nothing) :| []
       , conditions = Just (Compare Eq "age" (Lit (F 35)))
       })
    , testParse "SELECT name FROM users WHERE age = 35.5"
      (QS OldSelect
       { table = "users"
       , columns = CRef (ColumnRef (Var "name") Nothing) :| []
       , conditions = Just (Compare Eq "age" (Lit (F 35.5)))
       })
    , testParse "SELECT foo FROM bar WHERE baz > -2"
      (QS OldSelect
          { table = "bar"
          , columns = CRef (ColumnRef (Var "foo") Nothing) :| []
          , conditions = Just (Compare GT "baz" (Lit (F (-2) )))
          })
    , testParse "SELECT foo FROM bar WHERE baz = 2e-2"
        (QS OldSelect
          { table = "bar"
          , columns = CRef (ColumnRef (Var "foo") Nothing) :| []
          , conditions = Just (Compare Eq "baz" (Lit (F 0.02)))
          })
    , testParse "SELECT foo FROM bar WHERE baz = 2E-2"
        (QS OldSelect
          { table = "bar"
          , columns = CRef (ColumnRef (Var "foo") Nothing) :| []
          , conditions = Just (Compare Eq "baz" (Lit (F 0.02)))
          })
    , testParseExpr "2 * 3 + 1"
      (BinOp Add (BinOp Mul (Lit (F 2)) (Lit (F 3))) (Lit (F 1)))
    , testParseExpr "1 + 2 * 3"
      (BinOp Add (Lit (F 1)) (BinOp Mul (Lit (F 2)) (Lit (F 3))) )
    , testCase "lex '' escape" $
      assertEqual "" (Right [L.String "foo'bar", L.EOF]) (L.testLex "'foo''bar'")
    , testCase "lex semicolon" $
      assertEqual "" (Right [L.SELECT, L.Number 2.0, L.Add, L.Number 3.0, L.Semicolon, L.EOF]) (L.testLex "SELECT 2 + 3;")
    , testParse "SELECT foo FROM bar WHERE baz = 2E-2;"
      (QS OldSelect
          { table = "bar"
          , columns = CRef (ColumnRef (Var "foo") Nothing) :| []
          , conditions = Just (Compare Eq "baz" (Lit (F 0.02)))
          })
    ]

testParse :: TestName -> Query -> TestTree
testParse query expected = testCase query $
    assertEqual "" (Right expected) (parseQuery "<testcase>" query)

testParseExpr :: TestName -> Expr -> TestTree
testParseExpr query expected = testCase query $
    assertEqual "" (Right expected) (parseExpr "<testcase>" query)

antiquotes :: TestTree
antiquotes = testGroup "antiquotes"
    [ testCase "numberAntiquotes, Syntax" $
        assertEqual ""
            (QS (OldSelect {table =  "baz", columns = Var  "foo" :| [Var "bar"], conditions = Just (Compare Eq "foo" (NumberedParam 1 []))}), AntiquoteState 1 ["foo0"])
            (Syntax.numberAntiquotes 0 (QS (OldSelect {table =  "baz", columns = Var "foo" :| [Var "bar"], conditions = Just (Compare Eq "foo" (HaskellParam "foo0"))})))
    , testCase "numberAntiquotes, Raw" $
        assertEqual ""
            ("SELECT foo, bar FROM baz WHERE foo = $1", ["foo0"])
            (Raw.numberAntiquotes 0 [ Raw.Sql "SELECT foo, bar FROM baz WHERE foo = ", Raw.HaskellParam "foo0" ])
    ]
