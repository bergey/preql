{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Test.Syntax.Parser where

import Preql.QuasiQuoter.Syntax.Name
import Preql.QuasiQuoter.Syntax.Parser
import Preql.QuasiQuoter.Syntax.Syntax
import qualified Preql.QuasiQuoter.Syntax.Lex as L

import Data.List.NonEmpty (NonEmpty(..))
import Prelude hiding (Ordering(..), lex)
import Test.Tasty
import Test.Tasty.HUnit

parser :: TestTree
parser = testGroup "parser"
    [ testCase "lex '' escape" $
      assertEqual "" (Right [L.String "foo'bar", L.EOF]) (L.testLex "'foo''bar'")
    , testCase "lex semicolon" $
      assertEqual "" (Right [L.SELECT, L.Number 2.0, L.Add, L.Number 3.0, L.Semicolon, L.EOF]) (L.testLex "SELECT 2 + 3;")
    , testParseExpr "2 * 3 + 1"
      (BinOp Add (BinOp Mul (Lit (F 2)) (Lit (F 3))) (Lit (F 1)))
    , testParseExpr "1 + 2 * 3"
      (BinOp Add (Lit (F 1)) (BinOp Mul (Lit (F 2)) (Lit (F 3))) )
    , testParse "DELETE FROM taffy"
        (QD (Delete (mkName "taffy") Nothing))
    , testParse "dEleTe FROM taffy WHERE flavor = 'blueberry'"
      (QD Delete
          { table = mkName "taffy"
          , conditions = Just (BinOp (Comp Eq) (CRef "flavor") (Lit (T"blueberry")))
          })
    , testParse "DELETE FROM users WHERE email != 'bergey@teallabs.org'"
      (QD Delete
       { table = mkName "users"
       , conditions = Just (BinOp (Comp NEq) (CRef "email") (Lit (T "bergey@teallabs.org")))
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
      (QS (Simple select
       { from = [ TableRef "users" Nothing ]
       , targetList = [ Column (CRef "name") Nothing ]
       }))
    , testParse "SELECT name, email FROM users"
      (QS (Simple select
       { from = [ TableRef "users" Nothing ]
       , targetList = [ Column (CRef "name") Nothing, Column (CRef "email") Nothing ]
       }))
    , testParseExpr "name = 'Daniel'"
      (BinOp (Comp Eq) (CRef "name") (Lit (T "Daniel")))
    , testParse "SELECT name, email FROM users WHERE name = 'Daniel'"
      (QS (Simple select
       { from = [ TableRef "users" Nothing ]
       , targetList = [ Column (CRef "name") Nothing, Column (CRef "email") Nothing ]
       , whereClause = Just (BinOp (Comp Eq) (CRef "name") (Lit (T "Daniel")))
       }))
    , testParse "SELECT name, email FROM users WHERE name = 'Daniel' OR name = 'Bergey'"
      (QS (Simple select
       { from = [ TableRef "users" Nothing ]
       , targetList = [ Column (CRef "name") Nothing, Column (CRef "email") Nothing ]
       , whereClause = Just (Or (BinOp (Comp Eq) (CRef "name") (Lit (T "Daniel"))) (BinOp (Comp Eq) (CRef "name") (Lit (T "Bergey"))))
       }))
    , testParse "SELECT name FROM users WHERE age = 35"
        -- We currently parse integers & decimals all to Double
        -- Just test that both parser rules work
      (QS (Simple select
       { from = [ TableRef "users" Nothing ]
       , targetList = [ Column (CRef "name") Nothing ]
       , whereClause = Just (BinOp (Comp Eq) (CRef "age") (Lit (F 35)))
       }))
    , testParse "SELECT name FROM users WHERE age = 35.5"
      (QS (Simple select
       { from = [ TableRef "users" Nothing ]
       , targetList = [ Column (CRef "name") Nothing ]
       , whereClause = Just (BinOp (Comp Eq) (CRef "age") (Lit (F 35.5)))
       }))
    , testParse "SELECT foo FROM bar WHERE baz > -2"
      (QS (Simple select
       { from = [ TableRef "bar" Nothing ]
       , targetList = [ Column (CRef "foo") Nothing ]
       , whereClause = Just (BinOp (Comp GT) (CRef "baz") (Lit (F (-2))))
       }))
    , testParse "SELECT foo FROM bar WHERE baz = 2e-2"
      (QS (Simple select
       { from = [ TableRef "bar" Nothing ]
       , targetList = [ Column (CRef "foo") Nothing ]
       , whereClause = Just (BinOp (Comp Eq) (CRef "baz") (Lit (F 0.02)))
       }))
    , testParse "SELECT foo FROM bar WHERE baz = 2E-2"
      (QS (Simple select
       { from = [ TableRef "bar" Nothing ]
       , targetList = [ Column (CRef "foo") Nothing ]
       , whereClause = Just (BinOp (Comp Eq) (CRef "baz") (Lit (F 0.02)))
       }))
    , testParse "SELECT * FROM foobar"
      (QS (Simple select
       { from = [ TableRef "foobar" Nothing ]
       , targetList = [ Star ]
       }))
    , testParseExpr "TRUE" (Lit (B True))
    , testParseExpr "true" (Lit (B True))
    , testParseExpr "false" (Lit (B False))
    , testParse "SELECT * FROM foo ORDER BY bar"
      (QS (S (Simple select { from = [TableRef "foo" Nothing], targetList = [Star] })
          selectOptions { sortBy = [ SortBy (CRef "bar") (SortOrder DefaultSortOrder) NullsOrderDefault ] }))
    , testParse "SELECT * FROM foo ORDER BY bar DESC"
      (QS (S (Simple select { from = [TableRef "foo" Nothing], targetList = [Star] })
          selectOptions { sortBy = [ SortBy (CRef "bar") (SortOrder Descending) NullsOrderDefault ] }))
    , testParse "SELECT * FROM foo ORDER BY bar NULLS FIRST"
      (QS (S (Simple select { from = [TableRef "foo" Nothing], targetList = [Star] })
          selectOptions { sortBy = [ SortBy (CRef "bar") (SortOrder DefaultSortOrder) NullsFirst ] }))
    , testParse "SELECT * FROM foo ORDER BY bar ASC NULLS LAST"
      (QS (S (Simple select { from = [TableRef "foo" Nothing], targetList = [Star] })
          selectOptions { sortBy = [ SortBy (CRef "bar") (SortOrder Ascending) NullsLast ] }))
    , testParse "SELECT * FROM foobar LIMIT 5"
      (QS (S (Simple select { from = [TableRef "foobar" Nothing], targetList = [Star] })
          selectOptions { limit = Just (Lit (F 5)) }))
    , testParse "SELECT * FROM foobar LIMIT 5 OFFSET 5"
      (QS (S (Simple select { from = [TableRef "foobar" Nothing], targetList = [Star] })
          selectOptions { limit = Just (Lit (F 5)), offset = Just (Lit (F 5)) }))
    , testParse "SELECT * FROM foobar OFFSET 25"
      (QS (S (Simple select { from = [TableRef "foobar" Nothing], targetList = [Star] })
          selectOptions { offset = Just (Lit (F 25)) }))
    ]

testParse :: TestName -> Query -> TestTree
testParse query expected = testCase query $
    assertEqual "testParse" (Right expected) (parseQuery "<testcase>" query)

testParseExpr :: TestName -> Expr -> TestTree
testParseExpr query expected = testCase query $
    assertEqual "testParseExpr" (Right expected) (parseExpr "<testcase>" query)
