{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Construct FromSql instances

module Preql.FromSql.TH where

import Preql.FromSql.Class
import Preql.QuasiQuoter.Common (alphabet)
import Preql.Wire.Internal

import GHC.TypeNats
import Language.Haskell.TH

deriveFromSqlTuple :: Int -> Q [Dec]
deriveFromSqlTuple n = do
    names <- traverse newName (take n alphabet)
    let tupleT = foldl AppT (TupleT n) (map VarT names)
    return [fromSqlDecl names tupleT (tupleDataName n) n]

deriveFromSql :: Name -> Q [Dec]
deriveFromSql tyName = do
    info <- reify tyName
    case info of
        TyConI (DataD _cxt typeN binders _kind constructors _deriving) ->
            let
                tyVars = map tyVarName binders
                targetTy = foldl AppT (VarT typeN) (map VarT tyVars)
                (conN, fieldCount) = case constructors of
                    [NormalC conN elems] -> (conN, length elems)
                    [RecC conN fields] -> (conN, length fields)
                    [InfixC _ conN _] -> (conN, 2)
                    [_] -> error "deriveFromSql does not handle GADTs or constructors with class constraints"
                    _ -> error "deriveFromSql does not handle sum types"
            in return [fromSqlDecl tyVars targetTy conN fieldCount]
        _ -> error ("deriveFromSql only handles type names, got: " ++ show tyName)

tyVarName :: TyVarBndr -> Name
tyVarName = \case
    PlainTV name -> name
    KindedTV name _k -> name


fromSqlDecl :: [Name] -> Type -> Name -> Int -> Dec
fromSqlDecl tyVars targetTy constructor fieldCount =
    InstanceD Nothing context instanceHead [TySynInstD width, method] where
        context = [ ConT ''FromSql `AppT` VarT n | n <- tyVars ]
        instanceHead = ConT ''FromSql `AppT` targetTy
        width = TySynEqn Nothing
            (ConT ''Width `AppT` targetTy)
            (foldl (\a b -> ConT ''(+) `AppT` a `AppT` b) (LitT (NumTyLit 0))
                [ ConT ''Width `AppT` VarT n | n <- tyVars ])
        method = ValD
            (VarP 'fromSql)
            (NormalB (foldl
                      (\row field -> InfixE (Just row) (VarE 'applyDecoder) (Just field))
                      (VarE 'pureDecoder `AppE` ConE constructor)
                      (replicate fieldCount (VarE 'fromSql))))
            [] -- no where clause on the fromSql definition
