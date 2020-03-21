{-# LANGUAGE TemplateHaskell #-}
-- | Template Haskell macros to generate tuple instances for FromSql & ToSql

module Preql.Wire.Tuples where

import           Language.Haskell.TH

alphabet :: [String]
alphabet = cycle (map (:"") ['a'..'z'])

deriveFromSqlTuple :: Int -> Q [Dec]
deriveFromSqlTuple n = do
    names <- traverse newName (take n alphabet)
    Just classN <- lookupTypeName "FromSql"
    Just methodN <- lookupValueName "fromSql"
    let
        context = [ ConT classN `AppT` VarT n | n <- names ]
        instanceHead = ConT classN `AppT` foldl AppT (TupleT n) (map VarT names)
        method = ValD
            (VarP methodN)
            (NormalB (foldl
                      (\row field -> InfixE (Just row) (VarE '(<*>)) (Just field))
                      (VarE 'pure `AppE` ConE (tupleDataName n))
                      (replicate n (VarE methodN))))
            [] -- no where clause on the fromSql definition
    return [InstanceD Nothing context instanceHead [method]]

deriveToSqlTuple :: Int -> Q [Dec]
deriveToSqlTuple  n = do
    names <- traverse newName (take n alphabet)
    Just classN <- lookupTypeName "ToSql"
    Just fieldN <- lookupTypeName "ToSqlField"
    Just toSql <- lookupValueName "toSql"
    Just runFieldEncoder <- lookupValueName "runFieldEncoder"
    Just toSqlField <- lookupValueName "toSqlField"
    let
        context = [ ConT fieldN `AppT` VarT n | n <- names ]
        instanceHead = ConT classN `AppT` foldl AppT (TupleT n) (map VarT names)
        method = FunD toSql
            [Clause
                [TupP (map VarP names)]
                (NormalB (ListE [ VarE runFieldEncoder `AppE` VarE toSqlField `AppE` VarE n | n <- names ]))
            []] -- no where clause on the toSql definition
    return [InstanceD Nothing context instanceHead [method]]
