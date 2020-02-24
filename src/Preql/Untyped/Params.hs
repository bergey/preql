{-# LANGUAGE NamedFieldPuns #-}
-- | Parameter substitution for the Untyped SQL AST.

module Preql.Untyped.Params where

import Preql.Untyped.Syntax

import Control.Monad.Trans.State
import Data.Generics
import Data.Text (Text)
import Data.Vector (Vector, (!?))

-- | Replace all numbered parameter placeholders with values from the provided vector.
inlineParams :: Vector Text -> Query -> Query
inlineParams params = -- apply inlineParamsExpr to the Expr terms
    everywhere (mkT (inlineParamsExpr params))

inlineParamsExpr :: Vector Text -> Expr -> Expr
inlineParamsExpr params (NumberedParam i) =
    case params !? fromIntegral (i - 1) of
        Just txt -> InlineParam txt
        Nothing -> error $ "not enough parameters provided, needed at least " <> show i
inlineParamsExpr _ (HaskellParam _) = error "must replace antiquotes by numbered params before inlining values"
inlineParamsExpr _ expr = expr

numberAntiquotes :: Word -> Query -> (Query, AntiquoteState)
numberAntiquotes count q =
    let (rewritten, aqs) = runState
                   (everywhereM (mkM numberAntiquotesExpr) q)
                   (AntiquoteState count [])
    in (rewritten, aqs { haskellExpressions = reverse (haskellExpressions aqs) })

numberAntiquotesExpr :: Expr -> State AntiquoteState Expr
numberAntiquotesExpr (HaskellParam txt) = do
    AntiquoteState { paramCount, haskellExpressions } <- get
    let i = paramCount + 1
    put (AntiquoteState i (txt : haskellExpressions))
    return (NumberedParam i)
numberAntiquotesExpr expr = return expr

-- invariant: paramCount = length haskellExpressions
data AntiquoteState = AntiquoteState
    { paramCount :: Word
    , haskellExpressions :: [Text]
    } deriving (Show, Eq, Ord)

initialAntiquoteState :: AntiquoteState
initialAntiquoteState = AntiquoteState 0 []

-- | Return the highest-numbered $1-style parameter.
maxParam :: Query -> Word
maxParam = everything (+) (mkQ 0 maxParamExpr)

maxParamExpr :: Expr -> Word
maxParamExpr expr = case expr of
    NumberedParam i -> i
    InlineParam _ -> 0
    HaskellParam _ -> 0
    BinOp _ l r     -> max (maxParamExpr l) (maxParamExpr r)
    Unary _ e       -> maxParamExpr e
    Lit _           -> 0
    Var _           -> 0
