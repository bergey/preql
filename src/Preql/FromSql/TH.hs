{-# LANGUAGE CPP #-}
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
    let
      fields = map VarT names
      tuple = foldl AppT (TupleT n) fields
    return [fromSqlDecl tuple (tupleDataName n) fields]

deriveFromSql :: Name -> Q [Dec]
deriveFromSql tyName = do
    info <- reify tyName
    case info of
        TyConI (DataD _cxt typeN binders _kind constructors _deriving) ->
            let
                tyVars = map tyVarName binders
                targetTy = foldl AppT (ConT typeN) (map VarT tyVars)
                (conN, fieldTypes) = case constructors of
                    [NormalC con elems] -> (con, [ty | (_, ty) <- elems])
                    [RecC con fields] -> (con, [ty | (_, _, ty) <- fields])
                    [InfixC (_, t1) con (_, t2)] -> (con, [t1, t2])
                    [_] -> error "deriveFromSql does not handle GADTs or constructors with class constraints"
                    _ -> error "deriveFromSql does not handle sum types"
            in return [fromSqlDecl targetTy conN fieldTypes]
        _ -> error ("deriveFromSql only handles type names, got: " ++ show tyName)

tyVarName :: TyVarBndr -> Name
tyVarName = \case
    PlainTV name -> name
    KindedTV name _k -> name


fromSqlDecl :: Type -> Name -> [Type] -> Dec
fromSqlDecl targetTy constructor fields =
    InstanceD Nothing context instanceHead [TySynInstD width, method] where
        context = [ ConT ''FromSql `AppT` ty | ty <- fields, hasTyVar ty ]
        instanceHead = ConT ''FromSql `AppT` targetTy
        width = TySynEqn Nothing
            (ConT ''Width `AppT` targetTy)
            (foldl (\a b -> ConT ''(+) `AppT` a `AppT` b) (LitT (NumTyLit 0))
                [ ConT ''Width `AppT` ty | ty <- fields ])
        method = ValD
            (VarP 'fromSql)
            (NormalB (foldl
                      (\rowDecoder field -> InfixE (Just rowDecoder) (VarE 'applyDecoder) (Just field))
                      (VarE 'pureDecoder `AppE` ConE constructor)
                      (replicate (length fields) (VarE 'fromSql))))
            [] -- no where clause on the fromSql definition

hasTyVar :: Type -> Bool
hasTyVar = \case
  VarT _ -> True
  ForallT _ _ ty -> hasTyVar ty
#if MIN_VERSION_template_haskell(2,16,0)
  ForallVisT _ ty -> hasTyVar ty
#endif
  AppT t1 t2 -> hasTyVar t1 || hasTyVar t2
  AppKindT ty _ -> hasTyVar ty
  SigT ty _ -> hasTyVar ty
  InfixT t1 _ t2 -> hasTyVar t1 || hasTyVar t2
  UInfixT t1 _ t2 -> hasTyVar t1 || hasTyVar t2
  ParensT ty -> hasTyVar ty
  ImplicitParamT _ ty -> hasTyVar ty
  _ -> False
