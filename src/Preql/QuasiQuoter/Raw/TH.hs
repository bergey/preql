{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TemplateHaskell          #-}

module Preql.QuasiQuoter.Raw.TH where

import Preql.QuasiQuoter.Common
import Preql.QuasiQuoter.Raw.Lex (Token(..), parseQuery, unLex)
import Preql.Wire (Query)

import Data.String (IsString(..))
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Convert a rewritten SQL string to a ByteString, leaving width free
makeQuery :: String -> Q Exp
makeQuery string = [e|(fromString string :: Query $(VarT <$> (newName "n"))) |]

-- | Given a SQL query with ${} antiquotes, splice a pair @(Query
-- p r, p)@ or a function @\p' -> (Query p r, p)@ if the SQL
-- string includes both antiquote and positional parameters.

-- | The @sql@ Quasiquoter allows passing parameters to a query by name, inside a @${}@ antiquote.  For example:
-- @[rawSql| SELECT name, age FROM cats WHERE age >= ${minAge} and age < ${maxAge} |]@
-- The Haskell term within @{}@ must be a variable in scope; more complex expressions are not supported.
--
-- Antiquotes are replaced by positional (@$1, $2@) parameters supported by Postgres, and the
-- encoded values are sent with @PexecParams@
--
-- Mixed named & numbered parameters are also supported.  It is hoped that this will be useful when
-- migrating existing queries.  For example:
-- @query $ [rawSql| SELECT name, age FROM cats WHERE age >= ${minAge} and age < $1 |] maxAge@
-- Named parameters will be assigned numbers higher than the highest numbered paramater placeholder.
--
-- A quote with only named parameters is converted to a tuple '(Query, p)'.  For example:
-- @("SELECT name, age FROM cats WHERE age >= $1 and age < $2", (minAge, maxAge))@
-- If there are no parameters, the inner tuple is @()@, like @("SELECT * FROM cats", ())@.
-- If there are both named & numbered params, the splice is a function taking a tuple and returning
-- @(Query, p)@ where p includes both named & numbered params.  For example:
-- @\a -> ("SELECT name, age FROM cats WHERE age >= $1 and age < $2", (a, maxAge))@
rawSql :: QuasiQuoter
rawSql  = expressionOnly "rawSql" $ \raw -> do
    loc <- location
    let e_ast = parseQuery (show loc) raw
    case e_ast of
        Right parsed -> do
            let
                positionalCount = maxParam parsed
                (rewritten, haskellExpressions) = numberAntiquotes positionalCount parsed
                -- mkName, because we intend to capture what's in scope
                antiNames = map mkName haskellExpressions
            query <- makeQuery rewritten
            case positionalCount of
                0 -> -- only antiquotes (or no params)
                    return $ tupleE [query, tupleOrSingle antiNames]
                1 -> do -- one positional param, doesn't take a tuple
                    patternName <- newName "c"
                    return $ LamE [VarP patternName]
                        (tupleE [query, tupleOrSingle (patternName : antiNames)])
                _ -> do -- at least two positional parameters
                    patternNames <- cNames 'q' (fromIntegral positionalCount)
                    return $ LamE
                        [TupP (map VarP patternNames)]
                        (tupleE [query, tupleOrSingle (patternNames ++ antiNames)])
        Left err -> error err

maxParam :: [Token] -> Word
maxParam = foldr nextParam 0 where
  nextParam token maxSoFar =
      case token of
          NumberedParam i -> max i maxSoFar
          _               -> maxSoFar

numberAntiquotes :: Word -> [Token] -> (String, [String])
numberAntiquotes mp tokens = (concat sqlStrings, variableNames) where
  (sqlStrings, variableNames) = go mp tokens
  go _maxSoFar [] = ([], [])
  go maxSoFar (token : ts) =
      case token of
          HaskellParam name -> let
              newParam = maxSoFar + 1
              (ss, ns) = go newParam ts
              in (unLex (NumberedParam newParam) : ss, name : ns)
          EOF -> go maxSoFar ts
          _ -> let (ss, ns) = go maxSoFar ts in (unLex token : ss, ns)
