-- | Parameter substitution for the Untyped SQL AST.

module Untyped.Params where

import           Untyped.Syntax

import           Data.Generics
import           Data.Text      (Text)
import           Data.Vector    (Vector, (!?))

-- | Replace all numbered parameter placeholders with values from the provided vector.
inlineParams :: Vector Text -> Query -> Query
inlineParams params = -- apply this to the Expr terms
    everywhere (mkT (inlineParamsExpr params))

inlineParamsExpr :: Vector Text -> Expr -> Expr
inlineParamsExpr params (NumberedParam i) =
    case params !? fromIntegral (i - 1) of
        Just txt -> InlineParam txt
        Nothing -> error $ "not enough parameters provided, needed at least " <> show i
inlineParamsExpr _ expr = expr
