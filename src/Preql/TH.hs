{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TemplateHaskell          #-}

module Preql.TH where

import Preql.Untyped.Query
import Preql.Untyped.Params
import Preql.Untyped.Parser (parseQuery)
import qualified Preql.Untyped.Syntax as Syntax

import Data.String (IsString (..))
import Database.PostgreSQL.Simple (Only (..))
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift (..))

import qualified Data.Text as T

cNames :: Char -> Int -> Q [Name]
cNames c n = traverse newName (replicate n (c : ""))

tupleType :: [Name] -> Type
tupleType [v] = AppT (ConT ''Only) (VarT v)
tupleType names = foldl (\expr v -> AppT expr (VarT v)) (TupleT n) names
    where n = length names

-- | Synthesize a Query tagged with tuples of the given size
makeArityQuery :: String -> Syntax.Query -> Word -> Int -> Q Exp
makeArityQuery raw parsed p r =
    [e|Query raw parsed :: Query params result |]
    where
        params = tupleType <$> cNames 'p' (fromIntegral p)
        result = tupleType <$> cNames 'r' r

-- | Given a SQL query with ${} antiquotes, splice a pair @(Query
-- p r, p)@ or a function @\p' -> (Query p r, p)@ if the SQL
-- string includes both antiquote and positional parameters.
aritySql  :: QuasiQuoter
aritySql  = expressionOnly "aritySql " $ \raw -> do
    loc <- location
    let e_ast = parseQuery (show loc) raw
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
                    return $ TupE [typedQuery, tupleOrOnly antiNames]
                1 -> do -- one positional param, doesn't take a tuple
                    patternName <- newName "c"
                    return $ LamE [VarP patternName]
                        (TupE [typedQuery, tupleOrOnly (patternName : antiNames)])
                _ -> do -- at least one positional parameter
                    patternNames <- cNames 'q' (fromIntegral positionalCount)
                    return $ LamE
                        [TupP (map VarP patternNames)]
                        (TupE [typedQuery, tupleOrOnly (patternNames ++ antiNames)])
        Left err -> error err

tupleOrOnly :: [Name] -> Exp
tupleOrOnly names = case names of
    [name] -> AppE (ConE (mkName "Only")) (VarE name)
    vs -> TupE $ map VarE vs

expressionOnly :: String -> (String -> Q Exp) -> QuasiQuoter
expressionOnly name qq = QuasiQuoter
    { quoteExp = qq
    , quotePat = \_ -> error $ "qq " ++ name ++ " cannot be used in pattern context"
    , quoteType = \_ -> error $ "qq " ++ name ++ " cannot be used in type context"
    , quoteDec = \_ -> error $ "qq " ++ name ++ " cannot be used in declaration context"
    }

countColumnsReturned :: Syntax.Query -> Int
countColumnsReturned (Syntax.QS (Syntax.Select {columns})) = length columns
countColumnsReturned _                       = 0
