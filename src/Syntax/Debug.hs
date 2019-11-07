{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Syntax.Debug where

import           Syntax.Lex         (LocToken (..), alexMonadScan, runAlex)
import           Syntax.Name
import           Syntax.Parser
import           Syntax.Printer
import           Syntax.Untyped

import           Control.Monad      (liftM)
import           Data.List.NonEmpty (NonEmpty (..))
import           Prelude            hiding (Ordering (..), lex)

import qualified Syntax.Lex         as L
