{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Syntax.Debug where

import Syntax.Internal
import Syntax.Parser
import Syntax.Printer
import Syntax.Untyped
import Syntax.Lex (runAlex, alexMonadScan, LocToken(..))

import Control.Monad (liftM)
import Data.List.NonEmpty (NonEmpty(..))
import Prelude hiding (Ordering(..), lex)

import qualified Syntax.Lex as L

lexAll :: L.Alex [LocToken]
lexAll = do
    token <- alexMonadScan
    case unLoc token of
        L.EOF -> return [token]
        _ -> liftM (token :) lexAll

testLex' :: String -> Either String [LocToken]
testLex' s = runAlex s lexAll

testLex :: String -> Either String [L.Token]
testLex s = map unLoc <$> testLex' s
