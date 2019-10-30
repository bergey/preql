{-# LANGUAGE TemplateHaskell #-}
module TH where

import Language.Haskell.TH

a1Names :: Int -> [String]
a1Names n = take n names where
  names = [c : "" | c <- ['a'..'z']] ++ [ c : show i | i <- [1..], c <- ['a'..'z'] ]

tup2 :: Q Exp
tup2 = do
    a <- newName "a"
    b <- newName "b"
    return $ TupE [ VarE a, VarE b ]

tup2Ty :: Q Type
tup2Ty = do
    a <- newName "a"
    b <- newName "b"
    return $ -- ForallT [PlainTV a, PlainTV b] [] $
        AppT (AppT (TupleT 2) (VarT a)) (VarT b)

showTup2 :: Q Exp
showTup2 = do
    ta <- newName "ta"
    tb <- newName "tb"
    va <- newName "va"
    vb <- newName "vb"
    -- impl <- [e| \(a, b) -> show a ++ show b |]
    -- let impl = LamE [TupP [VarP va,VarP vb]] (InfixE (Just (AppE (VarE 'show) (VarE va))) (VarE '(++)) (Just (AppE (VarE 'show) (VarE vb))))
    impl <- [e| \(a, b) -> concat [show a, show b] |]
    return $ SigE impl
        (ForallT [PlainTV ta, PlainTV tb]
         [AppT (ConT ''Show) (VarT ta), AppT (ConT ''Show) (VarT tb)]
         (AppT (AppT ArrowT
                (AppT (AppT (TupleT 2) (VarT ta)) (VarT tb)))
             (ConT ''String)))

showTup :: Int -> Q Exp
showTup n = do
    typeNames <- traverse newName (map ('t':) (a1Names n))
    varNames <- traverse newName (map ('v':) (a1Names n))
    let
        impl = LamE
            [TupP (map VarP varNames)]
            (AppE (VarE 'concat) (ListE [AppE (VarE 'show) (VarE v) | v <- varNames]))
        tyVars = map PlainTV typeNames
        constraints = [AppT (ConT ''Show) (VarT v) | v<- typeNames]
        functionType = AppT (AppT ArrowT tupleType) (ConT ''String)
        tupleType = foldl (\expr v -> AppT expr (VarT v)) (TupleT n) typeNames
    return $ SigE impl (ForallT tyVars constraints functionType)
