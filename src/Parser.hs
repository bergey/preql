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
    otherwise -> Left ("No parse for " <> T.pack (show otherwise))
