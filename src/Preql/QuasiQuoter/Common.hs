{-# LANGUAGE CPP #-}
-- | Description: Functions for defining quasiquoters
--  used in both the SQL-validating quasiquoter and the simple non-validating QQ.

module Preql.QuasiQuoter.Common where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | A list of n Names beginning with the given character
cNames :: Char -> Int -> Q [Name]
cNames c n = traverse newName (replicate n (c : ""))

tupleOrSingle :: [Name] -> Exp
tupleOrSingle names = case names of
    [name] -> VarE name
    vs -> tupleE $ map VarE vs

expressionOnly :: String -> (String -> Q Exp) -> QuasiQuoter
expressionOnly name qq = QuasiQuoter
    { quoteExp = qq
    , quotePat = \_ -> error $ "qq " ++ name ++ " cannot be used in pattern context"
    , quoteType = \_ -> error $ "qq " ++ name ++ " cannot be used in type context"
    , quoteDec = \_ -> error $ "qq " ++ name ++ " cannot be used in declaration context"
    }

alphabet :: [String]
alphabet = cycle (map (:"") ['a'..'z'])

tupleE :: [Exp] -> Exp
#if MIN_VERSION_template_haskell(2,16,0)
tupleE = TupE . map Just
#else
tupleE = TupE
#endif
