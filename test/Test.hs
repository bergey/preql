{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeApplications         #-}

import           Connection
import           FromSql
import           Internal                  (Name, mkName)
import           Lex                       (alexScanTokens)
import           Parser
import           Printer
import           Query
import           Syntax

import           Control.Concurrent.MVar
import           Data.Either
import           Data.Int
import           Data.List.NonEmpty        (NonEmpty (..))
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.List.NonEmpty        as NE
import qualified Data.Text                 as T
import qualified Database.PostgreSQL.LibPQ as PQ

main :: IO ()
main = defaultMain $ testGroup "crispy-broccoli"
    [ parser
    , printer
    , integration
    ]

integration :: TestTree
integration = testGroup "integration"
    [ testCase "SELECT integer literal, raw PQ" $ do
        conn <- connect database
        connRaw <- takeMVar (connectionHandle conn)
        Just result <- PQ.exec connRaw "SELECT 2;"
        assertEqual "ntuples" (PQ.Row 1) =<< PQ.ntuples result
        assertEqual "nfields" (PQ.Col 1) =<< PQ.nfields result
        assertEqual "oid" (PQ.Oid 23) =<< PQ.ftype result (PQ.Col 0) -- int4
        assertEqual "value" (Just "2") =<< PQ.getvalue result (PQ.Row 0) (PQ.Col 0)
    , testCase "runQuery" $ do
        conn <- connect database
        assertEqual "" (Right [2]) =<< runQuery @() @Int32 conn  "SELECT 2" ()
    , testCase "schema type mismatch causes runtime error" $ do
        conn <- connect database
        assertBool "" . isLeft =<< runQuery @() @Int32 conn "SELECT 2.5" ()
    , testCase "ignore later columns" $ do
        conn <- connect database
        assertEqual "" (Right [2]) =<< runQuery @() @Int32 conn "SELECT 2, 3" ()
    , testCase "parse a pair" $ do
        conn <- connect database
        assertEqual "" (Right [(2, 3)]) =<< runQuery @() @(Int32, Int32) conn "SELECT 2, 3" ()
    , testCase "add Int32 parameters" $ do
        conn <- connect database
        assertEqual "" (Right [3]) =<< runQuery @(Int32, Int32) @Int32 conn "SELECT $1 + $2" (1, 2)
    , testCase "add Int64 parameters" $ do
        conn <- connect database
        assertEqual "" (Right [3]) =<< runQuery @(Int64, Int64) @Int64 conn "SELECT $1 + $2" (1, 2)
    , testCase "add Float32 parameters" $ do
        conn <- connect database
        assertEqual "" (Right [3]) =<< runQuery @(Float, Float) @Float conn "SELECT $1 + $2" (1, 2)
    , testCase "add Float64 parameters" $ do
        conn <- connect database
        assertEqual "" (Right [3]) =<< runQuery @(Double, Double) @Double conn "SELECT $1 + $2" (1, 2)
    , testCase "add 3 Int32 parameters" $ do
        conn <- connect database
        assertEqual "" (Right [6]) =<< runQuery @(Int32, Int32, Int32) @Int32 conn "SELECT $1 + $2 + $3" (1, 2, 3)
    ]

database :: ConnectInfo
database = ConnectInfo
    { connectHost = "localhost"
    , connectUser = "bergey"
    , connectDatabase = "crispy_broccoli"
    , connectPort = 5432
    , connectPassword = ""
    }

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
                  , conditions = Just (Op Eq (mkName "flavor") (Lit (T"blueberry")))
                  }))
    , testCase "INSERT, one column" $
        assertEqual ""
            "INSERT INTO users (email) VALUES ('bergey@teallabs.org')"
            (fmt (QI Insert
                 { table = mkName "users"
                 , columns = mkName "email" :| []
                 , values = T "bergey@teallabs.org" :| []
                 }))
    , testCase "INSERT, two columns" $
        assertEqual ""
            "INSERT INTO users (email, first_name) VALUES ('bergey@teallabs.org', 'Daniel')"
            (fmt (QI Insert
                 { table = mkName "users"
                 , columns = mkName "email" :| [ mkName "first_name" ]
                 , values = T "bergey@teallabs.org" :| [ T "Daniel" ]
                 }))
    ]

parser :: TestTree
parser = testGroup "parser"
    [ testParse "DELETE FROM taffy"
        (QD (Delete (mkName "taffy") Nothing))
    , testParse "dEleTe FROM taffy WHERE flavor = 'blueberry'"
      (QD Delete
          { table = mkName "taffy"
          , conditions = Just (Op Eq (mkName "flavor") (Lit (T"blueberry")))
          })
    , testParse "DELETE FROM users WHERE email != 'bergey@teallabs.org'"
      (QD Delete
       { table = mkName "users"
       , conditions = Just (Op NEq (mkName "email") (Lit (T "bergey@teallabs.org")))
       })
    , testParse "INSERT INTO users (email) VALUES ('bergey@teallabs.org')"
        (QI Insert
            { table = mkName "users"
            , columns = mkName "email" :| []
            , values = T "bergey@teallabs.org" :| []
            })
    ]

testParse query expected = testCase query $
    assertEqual "" (Right expected) (parse (alexScanTokens query))
