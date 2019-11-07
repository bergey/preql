{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Untyped.Debug where

import           Untyped.Lex        (LocToken (..), alexMonadScan, runAlex)
import           Untyped.Name
import           Untyped.Parser
import           Untyped.Printer
import           Untyped.Syntax

import           Control.Monad      (liftM)
import           Data.List.NonEmpty (NonEmpty (..))
import           Prelude            hiding (Ordering (..), lex)

import qualified Untyped.Lex        as L
