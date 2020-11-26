{-# LANGUAGE NamedFieldPuns #-}
-- | Parameter substitution for the Untyped SQL AST.

module Preql.QuasiQuoter.Syntax.Params where

import Preql.QuasiQuoter.Syntax.Syntax

import Control.Monad.Trans.State
import Data.Generics
import Data.Text (Text)

numberAntiquotes :: Word -> Statement -> (Statement, AntiquoteState)
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
maxParam :: Statement -> Word
maxParam = everything max (mkQ 0 maxParamExpr)

maxParamExpr :: Expr -> Word
maxParamExpr expr = case expr of
    NumberedParam i -> i
    HaskellParam _ -> 0
    BinOp _ l r     -> max (maxParamExpr l) (maxParamExpr r)
    Unary _ e       -> maxParamExpr e
    Lit _           -> 0
    CRef _ -> 0
    Indirection e _ -> maxParamExpr e
    SelectExpr stmt -> everything max (mkQ 0 maxParamExpr) stmt
    And l r -> max (maxParamExpr l) (maxParamExpr r)
    Or l r -> max (maxParamExpr l) (maxParamExpr r)
    Not e -> maxParamExpr e
    L likeE -> everything max (mkQ 0 maxParamExpr) likeE
    -- L LikeE {string, likePattern, escape} -> maybe id (max . maxParamExpr) escape
    --   (max (maxParamExpr string) (maxParamExpr likePattern))
    Fun f -> everything max (mkQ 0 maxParamExpr) f
    Cas cas -> everything max (mkQ 0 maxParamExpr) cas
