{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Main where

import Test.Syntax.RoundTrip
import Test.Syntax.Parser
import Test.Syntax.Printer
import Test.Wire (badConnection, connectionString, wire)

import Preql
import Preql.Imports
import Preql.QuasiQuoter.Raw.TH as Raw
import Preql.QuasiQuoter.Syntax.Params as Syntax (AntiquoteState(..), numberAntiquotes)
import Preql.QuasiQuoter.Syntax.Syntax hiding (query, select)
import qualified Preql.QuasiQuoter.Syntax.Syntax as Syntax
import qualified Preql.Wire.Query as W

import Control.Exception (throwIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Int
import GHC.TypeNats
import Prelude hiding (Ordering(..), lex)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Preql.QuasiQuoter.Raw.Lex as Raw

main :: IO ()
main = defaultMain $ testGroup "preql"
    [ antiquotes
    , wire
    , printer
    , lexer
    , parser
    , roundtrip
    , integration
    ]

integration :: TestTree
integration = withResource initDB W.finish $ \db ->
  let
    query' :: (ToSql p, FromSql r, KnownNat (Width r)) =>
      (Preql.Query (Width r), p) -> IO (Vector r)
    query' qp = runReaderT (query qp) =<< db
  in testGroup "integration"
    [ testCase "SELECT foo, bar FROM baz" $ do
        result <- query' [sql|SELECT foo, bar FROM baz |]
        assertEqual "" [(1, "one"), (2, "two")] (result :: Vector (Int32, T.Text))
    , testCase "with params" $ do
        result <- query' $ [select| SELECT foo, bar FROM baz WHERE foo = $1|] (1 :: Int32)
        assertEqual "" [(1, "one")] (result :: Vector (Int32, T.Text))
    , testCase "antiquote, 2 params" $ do
        let
            foo0 = 1 :: Int32
            bar0 = "one" :: T.Text
        result <- query' [select| SELECT foo, bar FROM baz WHERE foo = ${foo0} AND bar = ${bar0}|]
        assertEqual "" [(1, "one")] (result :: Vector (Int32, T.Text))
    , testCase "antiquote, 1 params" $ do
        let foo0 = 1 :: Int32
        result <- query' [select| SELECT foo, bar FROM baz WHERE foo = ${foo0}|]
        assertEqual "" [(1, "one")] (result :: Vector (Int32, T.Text))
    ]

initDB :: HasCallStack => IO Connection
initDB = do
    conn@(W.Connection rawConn _) <- W.connectdbSharedCache =<< connectionString
    status <- PQ.status rawConn
    unless (status == PQ.ConnectionOk) (throwIO =<< badConnection conn)
    let query' q = either throwIO return =<< W.query_ conn q ()
    query' "DROP TABLE IF EXISTS baz"
    query' "CREATE TABLE baz (foo int4, bar text)"
    query' "INSERT INTO baz (foo, bar) values (1, 'one'), (2, 'two')"
    return conn

antiquotes :: TestTree
antiquotes = testGroup "antiquotes"
    [ testCase "numberAntiquotes, Syntax" $
        assertEqual ""
            (QS (Simple Syntax.select
                 { from =  [ J (Table "baz") ]
                 , targetList = [ Column (CRef  "foo") Nothing, Column (CRef "bar") Nothing ]
                 , whereClause = Just (BinOp Eq (CRef "foo") (NumberedParam 1))
                 }), AntiquoteState 1 ["foo0"])
            (Syntax.numberAntiquotes 0 (QS (Simple Syntax.select
                 { from =  [ J (Table "baz") ]
                 , targetList = [ Column (CRef  "foo") Nothing, Column (CRef "bar") Nothing ]
                 , whereClause = Just (BinOp Eq (CRef "foo") (HaskellParam "foo0"))
                 })))
    , testCase "numberAntiquotes, Raw" $
        assertEqual ""
            ("SELECT foo, bar FROM baz WHERE foo = $1", ["foo0"])
            (Raw.numberAntiquotes 0 [ Raw.Sql "SELECT foo, bar FROM baz WHERE foo = ", Raw.HaskellParam "foo0" ])
    ]
