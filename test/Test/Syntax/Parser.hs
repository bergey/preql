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

lexer :: TestTree
lexer = testGroup "lexer"
    [ testLex "'foo''bar'" [L.String "foo'bar"]
    , testLex "SELECT 2 + 3;" [L.SELECT, L.Iconst 2, L.Add, L.Iconst 3, L.Semicolon]
    , testLex "5" [L.Iconst 5]
    , testLex "2.5" [L.Fconst 2.5]
    , testLex "2e-2" [L.Fconst 0.02]
    , testLex "2.5e-1" [L.Fconst 0.25]
    , testLex "1 2.3" [ L.Iconst 1, L.Fconst 2.3 ]
    , testLex "1 ." [ L.Iconst 1, L.Dot ]
    ]

parser :: TestTree
parser = testGroup "parser"
  [ testGroup "Expr"
    [ testParseExpr "2 * 3 + 1"
      (BinOp Add (BinOp Mul (Lit (I 2)) (Lit (I 3))) (Lit (I 1)))
    , testParseExpr "1 + 2 * 3"
      (BinOp Add (Lit (I 1)) (BinOp Mul (Lit (I 2)) (Lit (I 3))) )
    , testParseExpr "name = 'Daniel'"
      (BinOp (Comp Eq) (CRef "name") (Lit (T "Daniel")))
    , testParseExpr "TRUE" (Lit (B True))
    , testParseExpr "true" (Lit (B True))
    , testParseExpr "false" (Lit (B False))
    ]
  , testGroup "Query"
    [ testParse "DELETE FROM taffy"
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
    ]
  , testGroup "Select"
    [ testParse "SELECT name FROM users"
      (QS (Simple select
       { from = [ Table "users" ]
       , targetList = [ Column (CRef "name") Nothing ]
       }))
    , testParse "SELECT name, email FROM users"
      (QS (Simple select
       { from = [ Table "users" ]
       , targetList = [ Column (CRef "name") Nothing, Column (CRef "email") Nothing ]
       }))
    , testParse "SELECT name, email FROM users WHERE name = 'Daniel'"
      (QS (Simple select
       { from = [ Table "users" ]
       , targetList = [ Column (CRef "name") Nothing, Column (CRef "email") Nothing ]
       , whereClause = Just (BinOp (Comp Eq) (CRef "name") (Lit (T "Daniel")))
       }))
    , testParse "SELECT name, email FROM users WHERE name = 'Daniel' OR name = 'Bergey'"
      (QS (Simple select
       { from = [ Table "users" ]
       , targetList = [ Column (CRef "name") Nothing, Column (CRef "email") Nothing ]
       , whereClause = Just (Or (BinOp (Comp Eq) (CRef "name") (Lit (T "Daniel"))) (BinOp (Comp Eq) (CRef "name") (Lit (T "Bergey"))))
       }))
    , testParse "SELECT name FROM users WHERE age = 35"
        -- We currently parse integers & decimals all to Double
        -- Just test that both parser rules work
      (QS (Simple select
       { from = [ Table "users" ]
       , targetList = [ Column (CRef "name") Nothing ]
       , whereClause = Just (BinOp (Comp Eq) (CRef "age") (Lit (I 35)))
       }))
    , testParse "SELECT name FROM users WHERE age = 35.5"
      (QS (Simple select
       { from = [ Table "users" ]
       , targetList = [ Column (CRef "name") Nothing ]
       , whereClause = Just (BinOp (Comp Eq) (CRef "age") (Lit (F 35.5)))
       }))
    , testParse "SELECT foo FROM bar WHERE baz > -2"
      (QS (Simple select
       { from = [ Table "bar" ]
       , targetList = [ Column (CRef "foo") Nothing ]
       , whereClause = Just (BinOp (Comp GT) (CRef "baz") (Lit (I (-2))))
       }))
    , testParse "SELECT foo FROM bar WHERE baz = 2e-2"
      (QS (Simple select
       { from = [ Table "bar" ]
       , targetList = [ Column (CRef "foo") Nothing ]
       , whereClause = Just (BinOp (Comp Eq) (CRef "baz") (Lit (F 0.02)))
       }))
    , testParse "SELECT foo FROM bar WHERE baz = 2E-2"
      (QS (Simple select
       { from = [ Table "bar" ]
       , targetList = [ Column (CRef "foo") Nothing ]
       , whereClause = Just (BinOp (Comp Eq) (CRef "baz") (Lit (F 0.02)))
       }))
    , testParse "SELECT * FROM foobar"
      (QS (Simple select
       { from = [ Table "foobar" ]
       , targetList = [ Star ]
       }))
    , testParse "SELECT * FROM foo ORDER BY bar"
      (QS (S (Simple select { from = [Table "foo" ], targetList = [Star] })
          selectOptions { sortBy = [ SortBy (CRef "bar") (SortOrder DefaultSortOrder) NullsOrderDefault ] }))
    , testParse "SELECT * FROM foo ORDER BY bar DESC"
      (QS (S (Simple select { from = [Table "foo" ], targetList = [Star] })
          selectOptions { sortBy = [ SortBy (CRef "bar") (SortOrder Descending) NullsOrderDefault ] }))
    , testParse "SELECT * FROM foo ORDER BY bar NULLS FIRST"
      (QS (S (Simple select { from = [Table "foo" ], targetList = [Star] })
          selectOptions { sortBy = [ SortBy (CRef "bar") (SortOrder DefaultSortOrder) NullsFirst ] }))
    , testParse "SELECT * FROM foo ORDER BY bar ASC NULLS LAST"
      (QS (S (Simple select { from = [Table "foo" ], targetList = [Star] })
          selectOptions { sortBy = [ SortBy (CRef "bar") (SortOrder Ascending) NullsLast ] }))
    , testParse "SELECT * FROM foobar LIMIT 5"
      (QS (S (Simple select { from = [Table "foobar" ], targetList = [Star] })
          selectOptions { limit = Just (Lit (I 5)) }))
    , testParse "SELECT * FROM foobar LIMIT 5 OFFSET 5"
      (QS (S (Simple select { from = [Table "foobar" ], targetList = [Star] })
          selectOptions { limit = Just (Lit (I 5)), offset = Just (Lit (I 5)) }))
    , testParse "SELECT * FROM foobar OFFSET 25"
      (QS (S (Simple select { from = [Table "foobar" ], targetList = [Star] })
          selectOptions { offset = Just (Lit (I 25)) }))
    , testParse "TABLE foo"
      (QS (Simple select { from = [Table "foo" ], targetList = [Star] }))
    , testParse "SELECT foo FROM bar union SELECT baz FROM quux"
      (QS (Set Union Distinct
              (Simple select { from = [ Table "bar" ], targetList = [ Column (CRef "foo") Nothing ] })
              (Simple select { from = [ Table "quux" ], targetList = [ Column (CRef "baz") Nothing ] })))
    , testParse "SELECT foo FROM bar Intersect ALL SELECT baz FROM quux"
      (QS (Set Intersect All
              (Simple select { from = [ Table "bar" ], targetList = [ Column (CRef "foo") Nothing ] })
              (Simple select { from = [ Table "quux" ], targetList = [ Column (CRef "baz") Nothing ] })))
    , testParse "SELECT foo FROM bar EXCEPT SELECT baz FROM quux"
      (QS (Set Except Distinct
              (Simple select { from = [ Table "bar" ], targetList = [ Column (CRef "foo") Nothing ] })
              (Simple select { from = [ Table "quux" ], targetList = [ Column (CRef "baz") Nothing ] })))
    , testParse "SELECT * FROM foo CROSS JOIN bar"
      (QS (Simple select { from = [ CrossJoin (Table "foo") (Table "bar")]
                         , targetList = [Star] }))
    , testParse "SELECT * FROM foo JOIN bar ON foo.f = bar.b"
      (QS (Simple select { from = [ Join Inner (On (BinOp (Comp Eq) (CRef "foo.f") (CRef "bar.b"))) (Table "foo") (Table "bar")]
                         , targetList = [Star]}))
    , testParse "SELECT * FROM foo JOIN bar USING (f, b)"
      (QS (Simple select { from = [ Join Inner (Using ["f", "b"]) (Table "foo") (Table "bar")]
                         , targetList = [Star]}))
    , testParse "SELECT * FROM foo LEFT JOIN bar ON bar.foo = f.id"
      (QS (Simple select { from = [ Join LeftJoin (On (BinOp (Comp Eq) (CRef "bar.foo") (CRef "f.id"))) (Table "foo") (Table "bar")]
                         , targetList = [Star]}))
    , testParse "SELECT * FROM foo NATURAL JOIN bar"
      (QS (Simple select { from = [ Join Inner Natural (Table "foo") (Table "bar")]
                         , targetList = [Star]}))
    , testParse "SELECT DISTINCT foo FROM bar"
      (QS (Simple select { from = [ Table "bar" ], targetList = [ Column (CRef "foo") Nothing ], distinct = Just DistinctAll }))
    , testParse "SELECT DISTINCT ON (foo) * FROM bar"
      (QS (Simple select { from = [ Table "bar" ], targetList = [ Star ], distinct = Just (DistinctOn (CRef "foo" :| [] )) } ))
    , testParse "SELECT DISTINCT ON (foo, baz) * FROM bar"
      (QS (Simple select { from = [ Table "bar" ], targetList = [ Star ], distinct = Just (DistinctOn (CRef "foo" :| [CRef "baz" ] )) } ))
    , testParse "SELECT * FROM (SELECT foo FROM bar) AS baz"
      (QS (Simple select { targetList = [Star]
                         , from = [ SubSelect (Simple select { targetList = [ Column (CRef "foo") Nothing ], from = [ Table "bar" ] }) (Alias "baz" []) ] } ))
  , testParse "SELECT * FROM foo GROUP BY bar"
    (QS (Simple select { targetList = [Star], from = [Table "foo"]
                       , groupBy = [ CRef "bar" ] } ))
  -- TODO add count(*) or something, once we can parse functions
  , testParse "SELECT bar FROM foo GROUP BY bar HAVING count > 1"
    (QS (Simple select { targetList = [ Column (CRef "bar") Nothing ]
                       , from = [Table "foo"]
                       , groupBy = [ CRef "bar" ]
                       , having = Just (BinOp (Comp GT) (CRef "count") (Lit (I 1))) } ))
    ]
  ]

testParse :: TestName -> Query -> TestTree
testParse query expected = testCase query $
    assertEqual "testParse" (Right expected) (parseQuery "<testcase>" query)

testParseExpr :: TestName -> Expr -> TestTree
testParseExpr query expected = testCase query $
    assertEqual "testParseExpr" (Right expected) (parseExpr "<testcase>" query)

testLex :: TestName -> [L.Token] -> TestTree
testLex query expected = testCase query $
    assertEqual "testLex" (Right (expected ++ [L.EOF])) (L.testLex query)
