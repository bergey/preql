{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}

import Test.Wire (connectionString, wire)

import Preql.QuasiQuoter.Raw.TH
import Preql.Wire

import Data.Either
import Data.Int
import Data.List.NonEmpty (NonEmpty(..))
import Database.PostgreSQL.LibPQ (connectdb, finish)
import Prelude hiding (Ordering(..), lex)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Preql.QuasiQuoter.Raw.Lex as L
import qualified Preql.Wire.Query as W

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

main :: IO ()
main = defaultMain $ testGroup "crispy-broccoli"
    [ antiquotes
    , wire
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
    [ testCase "numberAntiquotes" $
        assertEqual ""
            ("SELECT foo, bar FROM baz WHERE foo = $1", ["foo0"])
            (numberAntiquotes 0 [ L.Sql "SELECT foo, bar FROM baz WHERE foo = ", L.HaskellParam "foo0" ])
    ]
