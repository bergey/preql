{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Preql.QuasiQuoter.Syntax.Debug where

import           Preql.QuasiQuoter.Syntax.Lex        (LocToken (..), alexMonadScan, runAlex)
import           Preql.QuasiQuoter.Syntax.Name
import           Preql.QuasiQuoter.Syntax.Parser
import           Preql.QuasiQuoter.Syntax.Printer
import           Preql.QuasiQuoter.Syntax.Syntax

import           Control.Monad      (liftM)
import           Data.List.NonEmpty (NonEmpty (..))
import           Prelude            hiding (Ordering (..), lex)

import qualified Preql.QuasiQuoter.Syntax.Lex        as L
