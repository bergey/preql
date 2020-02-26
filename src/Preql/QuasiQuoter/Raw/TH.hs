{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TemplateHaskell          #-}

module Preql.QuasiQuoter.Raw.TH where

import           Preql.QuasiQuoter.Raw.Lex (Token(..), unLex, parseQuery)
import           Preql.Wire (Query(..))

import           Data.String (IsString (..))
import           Data.Word (Word)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (Lift (..))

import qualified Data.Text as T

-- | A list of n Names beginning with the given character
cNames :: Char -> Int -> Q [Name]
cNames c n = traverse newName (replicate n (c : ""))

-- | Convert a rewritten SQL string to a ByteString
makeQuery :: String -> Q Exp
makeQuery string = [e|(fromString string :: Query) |]

-- | Given a SQL query with ${} antiquotes, splice a pair @(Query
-- p r, p)@ or a function @\p' -> (Query p r, p)@ if the SQL
-- string includes both antiquote and positional parameters.
sql  :: QuasiQuoter
sql  = expressionOnly "aritySql " $ \raw -> do
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
                    return $ TupE [query, tupleOrSingle antiNames]
                1 -> do -- one positional param, doesn't take a tuple
                    patternName <- newName "c"
                    return $ LamE [VarP patternName]
                        (TupE [query, tupleOrSingle (patternName : antiNames)])
                _ -> do -- at least two positional parameters
                    patternNames <- cNames 'q' (fromIntegral positionalCount)
                    return $ LamE
                        [TupP (map VarP patternNames)]
                        (TupE [query, tupleOrSingle (patternNames ++ antiNames)])
        Left err -> error err

tupleOrSingle :: [Name] -> Exp
tupleOrSingle names = case names of
    [name] -> VarE name
    vs -> TupE $ map VarE vs

expressionOnly :: String -> (String -> Q Exp) -> QuasiQuoter
expressionOnly name qq = QuasiQuoter
    { quoteExp = qq
    , quotePat = \_ -> error $ "qq " ++ name ++ " cannot be used in pattern context"
    , quoteType = \_ -> error $ "qq " ++ name ++ " cannot be used in type context"
    , quoteDec = \_ -> error $ "qq " ++ name ++ " cannot be used in declaration context"
    }

maxParam :: [Token] -> Word
maxParam = foldr nextParam 0 where
  nextParam token maxParam =
      case token of
          NumberedParam i -> max i maxParam
          _ -> maxParam

numberAntiquotes :: Word -> [Token] -> (String, [String])
numberAntiquotes mp ts = (concat sqlStrings, variableNames) where
  (sqlStrings, variableNames) = go mp ts
  go _maxParam [] = ([], [])
  go maxParam (token : ts) =
      case token of
          HaskellParam name -> let
              newParam = maxParam + 1
              (ss, ns) = go newParam ts
              in (unLex (NumberedParam newParam) : ss, name : ns)
          EOF -> go maxParam ts
          _ -> let (ss, ns) = go maxParam ts in (unLex token : ss, ns)
