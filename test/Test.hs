{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
import Syntax
import Printer
import Internal (Name, mkName)

import Data.List.NonEmpty (NonEmpty(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List.NonEmpty as NE

main :: IO ()
main = defaultMain $ testGroup "crispy-broccoli" [
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
