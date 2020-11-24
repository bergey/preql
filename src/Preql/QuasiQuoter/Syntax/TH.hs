{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TemplateHaskell          #-}
module Preql.QuasiQuoter.Syntax.TH where

import Preql.Imports
import Preql.QuasiQuoter.Common
import Preql.QuasiQuoter.Syntax.Params
import Preql.QuasiQuoter.Syntax.Parser (parseStatement, parseSelect)
import Preql.QuasiQuoter.Syntax.Printer (formatAsByteString)
import Preql.QuasiQuoter.Syntax.Syntax as Syntax hiding (select)
import Preql.Wire.Internal as Wire (Query(..))

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Data.Text as T

tupleType :: [Name] -> Type
tupleType [v] = VarT v
tupleType names = foldl (\expr v -> AppT expr (VarT v)) (TupleT n) names
    where n = length names

-- | Synthesize a Query tagged with the number of returned columns.
makeArityQuery :: Statement -> Q Exp
makeArityQuery parsed = do
  let
    width = case countColumnsReturned parsed of
              Just n -> pure (LitT (NumTyLit (fromIntegral n)))
              Nothing -> VarT <$> newName "r" -- SELECT *
    formatted = formatAsByteString parsed
  [e| Wire.Query formatted :: Wire.Query $(width) |]

-- | This quasiquoter will accept most syntactically valid SELECT
-- queries.  Language features not yet implemented include type casts,
-- lateral joins, EXTRACT, INTO, string & XML operators, and
-- user-defined operators.  For now, please fall back to
-- 'Preql.QuasiQuoter.Raw.TH.sql' for these less-frequently used SQL
-- features, or file a bug report if a commonly used feature is not
-- parsed correctly.
--
-- @select@ accepts antiquotes with the same syntax as 'sql'.
select :: QuasiQuoter
select = expressionOnly "select" (aritySql parseSelect QS)

-- | This quasiquoter will accept all queries accepted by 'select',
-- and limited INSERT, UPDATE, and DELETE queries.  For details of
-- what can be parsed, consult Parser.y
validSql :: QuasiQuoter
validSql = expressionOnly "validSql" (aritySql parseStatement id)

aritySql  :: (String -> String -> Either String a) -> (a -> Statement) -> String -> Q Exp
aritySql parse mkStatement raw = do
    loc <- location
    let e_ast = mkStatement <$> parse (show loc) raw
    case e_ast of
        Right parsed -> do
            let
                positionalCount = maxParam parsed
                (rewritten, aqs) = numberAntiquotes positionalCount parsed
                antiNames = map (mkName . T.unpack) (haskellExpressions aqs)
            typedQuery <- makeArityQuery rewritten
            case positionalCount of
                0 -> -- only antiquotes (or no params)
                    return $ tupleE [typedQuery, tupleOrSingle antiNames]
                1 -> do -- one positional param, doesn't take a tuple
                    patternName <- newName "c"
                    return $ LamE [VarP patternName]
                        (tupleE [typedQuery, tupleOrSingle (patternName : antiNames)])
                _ -> do -- at least two positional parameters
                    patternNames <- cNames 'q' (fromIntegral positionalCount)
                    return $ LamE
                        [TupP (map VarP patternNames)]
                        (tupleE [typedQuery, tupleOrSingle (patternNames ++ antiNames)])
        Left err -> error err

countColumnsReturned :: Statement -> Maybe Int
countColumnsReturned (QS selectQ) = go selectQ where
  go s = case s of
      SelectValues rows -> Just (foldl' max 0 (fmap length rows))
      Simple Select {targetList} -> if Star `elem` targetList
          then Nothing
          else Just (length targetList)
      S ss _ -> go ss
      Set _ _ a b -> case (go a, go b) of
        (Just m, Just n) | m == n -> Just n
        _ -> Nothing
countColumnsReturned _                       = Just 0
-- TODO INSERT ... RETURNING &c
