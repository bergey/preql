{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
import Syntax
import Printer
import Internal (Name, mkName)

import Test.Tasty
import Test.Tasty.HUnit

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
    ]
