{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Test.Syntax.Printer where

import Preql.QuasiQuoter.Syntax.Name
import Preql.QuasiQuoter.Syntax.Printer
import Preql.QuasiQuoter.Syntax.Syntax

import Data.List.NonEmpty (NonEmpty(..))
import Test.Tasty
import Test.Tasty.HUnit

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
            "DELETE FROM taffy WHERE (flavor) = ('blueberry')"
            (fmt (QD Delete
                  { table = mkName "taffy"
                  , conditions = Just (BinOp Eq (CRef "flavor") (Lit (T"blueberry")))
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
        -- Extra parens until the printer is clever about Expr precedence
        "SELECT name, email FROM users WHERE (name) = ($1)"
        (fmt (QS (Simple select
                  { from = [ Table "users" ]
                  , targetList = [ Column (CRef "name") Nothing, Column (CRef "email") Nothing ]
                  , whereClause = Just (BinOp Eq (CRef "name") (NumberedParam 1 []))
                  })))
    , testPrint "SELECT * FROM foobar LIMIT 5.0"
        (QS (S (Simple select { from = [Table "foobar" ], targetList = [Star] })
             selectOptions { limit = Just (Lit (F 5)) }))
    , testPrint "SELECT foo FROM bar UNION SELECT baz FROM quux"
      (QS (Set Union Distinct
              (Simple select { from = [ Table "bar" ], targetList = [ Column (CRef "foo") Nothing ] })
              (Simple select { from = [ Table "quux" ], targetList = [ Column (CRef "baz") Nothing ] })))
    , testPrint "SELECT * FROM (SELECT foo FROM bar) AS baz"
      (QS (Simple select { targetList = [Star]
                         , from = [ SubSelect (Simple select { targetList = [ Column (CRef "foo") Nothing ], from = [ Table "bar" ] }) (Alias "baz" []) ] } ))
    ]

testPrint :: TestName -> Query -> TestTree
testPrint expected query = testCase expected $
    assertEqual "testPrint" expected (formatAsString query)
