{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module TH where

import Syntax.Untyped as Syntax
import Syntax.Parser (parseExp)
import TypedQuery

import Data.String (IsString(..))
import Database.PostgreSQL.Simple (Only(..))
import Language.Haskell.TH
import Language.Haskell.TH.Quote

a1Names :: Int -> [String]
a1Names n = take n names where
  names = [c : "" | c <- ['a'..'z']] ++ [ c : show i | i <- [1..], c <- ['a'..'z'] ]

cNames :: Char -> Int -> Q [Name]
cNames c n = traverse newName [c : show i | i <- [1..n] ]

tupleType :: [Name] -> Type
tupleType [v] = AppT (ConT ''Only) (VarT v)
tupleType names = foldl (\expr v -> AppT expr (VarT v)) (TupleT n) names
    where n = length names

-- | Synthesize a TypedQuery tagged with tuples of the given size
makeArityQuery :: String -> Int -> Int -> Q Exp
makeArityQuery q p r = do
    paramNames <- cNames 'p' p
    resultNames <- cNames 'r' r
    return $ SigE
        (AppE (ConE 'TypedQuery) (AppE (VarE 'fromString) (LitE (StringL q))))
        (AppT (AppT (ConT ''TypedQuery) (tupleType paramNames)) (tupleType resultNames))

countColumnsReturned :: Syntax.Query -> Int
countColumnsReturned (QS (Select {columns})) = length columns
countColumnsReturned _ = 0

aritySql :: QuasiQuoter
aritySql = QuasiQuoter
    { quoteExp = \q -> do
            loc <- location
            let e_ast = parseExp (show loc) q
            case e_ast of
                Right ast -> makeArityQuery q 0 (countColumnsReturned ast)
                Left err -> error err
    , quotePat = \_ -> error "qq aritySql cannot be used in pattern context"
    , quoteType = \_ -> error "qq aritySql cannot be used in type context"
    , quoteDec = \_ -> error "qq aritySql cannot be used in declaration context"
    }
