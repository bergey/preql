{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

import           Instances
import           Test.Wire (wire, database)

import           Syntax.Internal (Name, mkName)
import           Syntax.Parser
import           Syntax.Printer
import           Syntax.Untyped
import           TH
import           TypedQuery

import           Data.Either
import           Data.Int
import           Data.List.NonEmpty (NonEmpty (..))
import           Database.PostgreSQL.Simple (connect, close)
import           Prelude hiding (Ordering(..), lex)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import qualified Syntax.Lex as L

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

main :: IO ()
main = defaultMain $ testGroup "crispy-broccoli"
    [ parser
    , printer
    , wire
    , quickCheck
    , integration
    ]

integration :: TestTree
integration = withResource (connect database) close $ \db -> testGroup "integration"
    [ testCase "SELECT foo, bar FROM baz" $ do
        conn <- db
        result <- runQuery conn [aritySql|SELECT foo, bar FROM baz |] ()
        assertEqual "" [(1, "one"), (2, "two")] (result :: [(Int, T.Text)])
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
        (fmt (QS Select
              { table = "users"
              , columns = Var "name" :| [ Var "email"]
              , conditions = Just (Compare Eq "name" (Param 1))
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
      (QS Select
       { table = "users"
       , columns = Var "name" :| []
       , conditions = Nothing
       })
    , testParse "SELECT name, email FROM users"
      (QS Select
       { table = "users"
       , columns = Var "name" :| [Var "email"]
       , conditions = Nothing
       })
    , testParse "SELECT name, email FROM users WHERE name = 'Daniel'"
      (QS Select
       { table = "users"
       , columns = Var "name" :| [Var "email"]
       , conditions = Just (Compare Eq "name" (Lit (T "Daniel")))
       })
    , testParse "SELECT name, email FROM users WHERE name = 'Daniel' OR name = 'Bergey'"
      (QS Select
       { table = "users"
       , columns = Var "name" :| [Var "email"]
       , conditions = Just (Or (Compare Eq "name" (Lit (T "Daniel"))) (Compare Eq "name" (Lit (T "Bergey"))))
       })
    , testParse "SELECT name FROM users WHERE age = 35"
        -- We currently parse integers & decimals all to Double
        -- Just test that both parser rules work
      (QS Select
       { table = "users"
       , columns = Var "name" :| []
       , conditions = Just (Compare Eq "age" (Lit (F 35)))
       })
    , testParse "SELECT name FROM users WHERE age = 35.5"
      (QS Select
       { table = "users"
       , columns = Var "name" :| []
       , conditions = Just (Compare Eq "age" (Lit (F 35.5)))
       })
    , testParse "SELECT foo FROM bar WHERE baz > -2"
      (QS Select
          { table = "bar"
          , columns = Var "foo" :| []
          , conditions = Just (Compare GT "baz" (Lit (F (-2) )))
          })
    , testParse "SELECT foo FROM bar WHERE baz = 2e-2"
        (QS Select
          { table = "bar"
          , columns = Var "foo" :| []
          , conditions = Just (Compare Eq "baz" (Lit (F (0.02))))
          })
    , testParse "SELECT foo FROM bar WHERE baz = 2E-2"
        (QS Select
          { table = "bar"
          , columns = Var "foo" :| []
          , conditions = Just (Compare Eq "baz" (Lit (F (0.02))))
          })
    , testParseExpr "2 * 3 + 1"
      (BinOp Add (BinOp Mul (Lit (F 2)) (Lit (F 3))) (Lit (F 1)))
    , testParseExpr "1 + 2 * 3"
      (BinOp Add (Lit (F 1)) (BinOp Mul (Lit (F 2)) (Lit (F 3))) )
    , testCase "lex '' escape" $
      assertEqual "" (Right [L.String "foo'bar", L.EOF]) (L.testLex "'foo''bar'")
    ]

testParse query expected = testCase query $
    assertEqual "" (Right expected) (parseQuery "<testcase>" query)

testParseExpr query expected = testCase query $
    assertEqual "" (Right expected) (parseExpr "<testcase>" query)

quickCheck :: TestTree
quickCheck = testGroup "QuickCheck"
    [ testProperty "Arbitrary SELECT" (\select -> assertRoundTrip (QS select))
    ]

assertRoundTrip :: Query -> Bool
assertRoundTrip query =
    Right query == parseQuery "<assertRoundTrip>" printed
  where printed = TL.unpack . TLB.toLazyText . fmt $ query
