{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

import Test.Wire (wire)
import Test.Syntax.Printer
import Test.Syntax.Parser

import Preql.QuasiQuoter.Raw.TH as Raw
import Preql.QuasiQuoter.Syntax.Params as Syntax (AntiquoteState(..), numberAntiquotes)
import Preql.QuasiQuoter.Syntax.Syntax

import Data.List.NonEmpty (NonEmpty(..))
import Prelude hiding (Ordering(..), lex)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Preql.QuasiQuoter.Raw.Lex as Raw

main :: IO ()
main = defaultMain $ testGroup "preql"
    [ antiquotes
    , wire
    , printer
    , parser
    -- , integration
    ]

-- integration :: TestTree
-- integration = withResource (connectdb connectionString) finish $ \db -> testGroup "integration"
    -- [ testCase "SELECT foo, bar FROM baz" $ do
    --     conn <- db
    --     result <- query conn [sql|SELECT foo, bar FROM baz |]
    --     assertEqual "" [(1, "one"), (2, "two")] (result :: [(Int, T.Text)])
    -- ]
--     , testCase "with params" $ do
--         conn <- db
--         result <- query conn $ [aritySql| SELECT foo, bar FROM baz WHERE foo = $1|] (1 :: Int)
--         assertEqual "" [(1, "one")] (result :: [(Int, T.Text)])
--     , testCase "antiquote, 2 params" $ do
--         conn <- db
--         let
--             foo0 = 1 :: Int
--             bar0 = "one" :: T.Text
--         result <- query conn [aritySql| SELECT foo, bar FROM baz WHERE foo = ${foo0} AND bar = ${bar0}|]
--         assertEqual "" [(1, "one")] (result :: [(Int, T.Text)])
--     , testCase "antiquote, 1 params" $ do
--         conn <- db
--         let foo0 = 1 :: Int
--         result <- query conn [aritySql| SELECT foo, bar FROM baz WHERE foo = ${foo0}|]
--         assertEqual "" [(1, "one")] (result :: [(Int, T.Text)])
--     ]

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
