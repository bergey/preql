{-# LANGUAGE TemplateHaskell #-}
-- | Template Haskell macros to generate tuple instances for FromSql & ToSql

module Preql.Wire.Tuples where

import           GHC.TypeNats
import           Language.Haskell.TH
import           Preql.Wire.Internal (applyDecoder, pureDecoder)

alphabet :: [String]
alphabet = cycle (map (:"") ['a'..'z'])

deriveFromSqlTuple :: Int -> Q [Dec]
deriveFromSqlTuple n = do
    names <- traverse newName (take n alphabet)
    -- We can't import these names becuse FromSql imports this module
    Just classN <- lookupTypeName "FromSql"
    Just methodN <- lookupValueName "fromSql"
    Just widthN <- lookupTypeName "Width"
    let
        context = [ ConT classN `AppT` VarT n | n <- names ]
        tupleT = foldl AppT (TupleT n) (map VarT names)
        instanceHead = ConT classN `AppT` tupleT
        width = TySynEqn Nothing
            (ConT widthN `AppT` tupleT)
            (foldl (\a b -> (ConT ''(+)) `AppT` a `AppT` b) (LitT (NumTyLit 0))
                [ ConT widthN `AppT` VarT n | n <- names ])

        method = ValD
            (VarP methodN)
            (NormalB (foldl
                      (\row field -> InfixE (Just row) (VarE 'applyDecoder) (Just field))
                      (VarE 'pureDecoder `AppE` ConE (tupleDataName n))
                      (replicate n (VarE methodN))))
            [] -- no where clause on the fromSql definition
    return [InstanceD Nothing context instanceHead [TySynInstD width, method]]

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
