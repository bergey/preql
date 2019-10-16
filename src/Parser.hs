{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Internal
import qualified Lex as L
import           Syntax

import           Data.Text (Text)

import qualified Data.Text as T

parse :: Text -> Either Text Query
parse txt = case L.alexScanTokens (T.unpack txt) of
    [ L.Delete, L.From, L.Name table ] -> Right (QD (Delete (mkName table) Nothing))
    [ L.Delete, L.From, L.Name table, L.Where, L.Name column, L.Equals, L.String value ] ->
        Right (QD (Delete (mkName table) (Just (Op Eq (mkName column) (Lit (T value))))))
    otherwise -> Left ("No parse for " <> T.pack (show otherwise))
