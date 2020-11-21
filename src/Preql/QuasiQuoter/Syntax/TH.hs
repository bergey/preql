{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TemplateHaskell          #-}
module Preql.QuasiQuoter.Syntax.TH where

import Preql.Imports
import Preql.QuasiQuoter.Common
import Preql.QuasiQuoter.Syntax.Params
import Preql.QuasiQuoter.Syntax.Parser (parseQuery, parseSelect)
import Preql.QuasiQuoter.Syntax.Syntax as Syntax hiding (select)
import Preql.QuasiQuoter.Syntax.TypedQuery

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Data.Text as T

tupleType :: [Name] -> Type
tupleType [v] = VarT v
tupleType names = foldl (\expr v -> AppT expr (VarT v)) (TupleT n) names
    where n = length names

-- TODO merge arity-qq branch so we can use any decoder of correct arity, not only tuples
-- | Synthesize a TypedQuery tagged with tuples of the given size
makeArityQuery :: String -> Query -> Word -> Maybe Int -> Q Exp
makeArityQuery raw parsed p r = do
  params <- tupleType <$> cNames 'p' (fromIntegral p)
  result <- case r of
              Just r' -> tupleType <$> cNames 'r' r'
              Nothing -> VarT <$> newName "r" -- SELECT *
  value <- [e|TypedQuery raw parsed |]
  return $ SigE value (ConT ''TypedQuery `AppT` params `AppT` result)

-- | Given a SQL SELECT query with ${} antiquotes, splice a pair
-- @(TypedQuery p r, p)@ or a function @\p' -> (TypedQuery p r, p)@ if
-- the SQL string includes both antiquote and positional parameters.
-- This quasiquoter will accept most syntactically valid SELECT
-- queries.  Language features not yet implemented include type casts,
-- lateral joins, EXTRACT, INTO, string & XML operators, and
-- user-defined operators.  For now, please fall back to
-- 'Preql.QuasiQuoter.Raw.TH.sql' for these less-frequently used SQL
-- features, or file a bug report if a commonly used feature is not
-- parsed correctly.
select :: QuasiQuoter
select = expressionOnly "select" (aritySql parseSelect QS)

-- | This quasiquoter will accept all queries accepted by 'select',
-- and limited INSERT, UPDATE, and DELETE queries.  For details of
-- what can be parsed, consult Parser.y
validSql :: QuasiQuoter
validSql = expressionOnly "validSql" (aritySql parseQuery id)

aritySql  :: (String -> String -> Either String a) -> (a -> Syntax.Query) -> String -> Q Exp
aritySql parse mkQuery raw = do
    loc <- location
    let e_ast = mkQuery <$> parse (show loc) raw
    case e_ast of
        Right parsed -> do
            let
                positionalCount = maxParam parsed
                (rewritten, aqs) = numberAntiquotes positionalCount parsed
                antiNames = map (mkName . T.unpack) (haskellExpressions aqs)
            typedQuery <- makeArityQuery raw rewritten
                (paramCount aqs)
                (countColumnsReturned rewritten)
            case positionalCount of
                0 -> -- only antiquotes (or no params)
                    return $ TupE [typedQuery, tupleOrSingle antiNames]
                1 -> do -- one positional param, doesn't take a tuple
                    patternName <- newName "c"
                    return $ LamE [VarP patternName]
                        (TupE [typedQuery, tupleOrSingle (patternName : antiNames)])
                _ -> do -- at least two positional parameters
                    patternNames <- cNames 'q' (fromIntegral positionalCount)
                    return $ LamE
                        [TupP (map VarP patternNames)]
                        (TupE [typedQuery, tupleOrSingle (patternNames ++ antiNames)])
        Left err -> error err

countColumnsReturned :: Syntax.Query -> Maybe Int
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
