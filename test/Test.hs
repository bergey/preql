{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import           Connection
import           Internal (Name, mkName)
import           Printer
import           Syntax

import           Control.Concurrent.MVar
import           Data.List.NonEmpty (NonEmpty(..))
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = defaultMain $ testGroup "crispy-broccoli"
    [ syntax
    , integration
    ]

integration :: TestTree
integration = testGroup "integration" [
    testCase "SELECT integer literal" $ do
        conn <- connect database
        connRaw <- takeMVar (connectionHandle conn)
        Just result <- PQ.exec connRaw "SELECT 2;"
        assertEqual "ntuples" (PQ.Row 1) =<< PQ.ntuples result
        assertEqual "nfields" (PQ.Col 1) =<< PQ.nfields result
        assertEqual "value" (Just "2") =<< PQ.getvalue result (PQ.Row 0) (PQ.Col 0)
    ]

database :: ConnectInfo
database = ConnectInfo
    { connectHost = "localhost"
    , connectUser = "bergey"
    , connectDatabase = "crispy_broccoli"
    , connectPort = 5432
    , connectPassword = ""
    }

-- | Tests of the SQL syntax parser
syntax :: TestTree
syntax = testGroup "parser" [
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
