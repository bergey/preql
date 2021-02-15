{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Construct FromSql instances

module Preql.FromSql.TH where

import Preql.FromSql.Class
import Preql.FromSql.Tuple
import Preql.QuasiQuoter.Common (alphabet)
import Preql.Wire.Errors (PgType(Oid))
import Preql.Wire.Internal
import qualified Preql.Wire.TypeInfo.Static as OID

import GHC.TypeNats
import Language.Haskell.TH
import qualified PostgreSQL.Binary.Decoding as PGB

-- | instance (FromSql a, FromSql b) => FromSql (a, b)
deriveFromSqlTuple :: Int -> Q [Dec]
deriveFromSqlTuple n = do
    names <- traverse newName (take n alphabet)
    let
      fields = map VarT names
      tuple = foldl AppT (TupleT n) fields
    return [fromSqlDecl tuple (tupleDataName n) fields]

-- | derive a 'FromSql' instance for a record type
-- (field names are not required, but there must be only one constructor)
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

-- instance (FromSqlField a, FromSqlField b) => FromSqlField (Tuple (a, b))
-- instance (FromSqlField a, FromSqlField b) => FromSql (Tuple (a, b))
deriveFromSqlFieldTuple :: Int -> Q [Dec]
deriveFromSqlFieldTuple n = do
  names <- traverse newName (take n alphabet)
  fieldOid <- [e| Oid OID.recordOid OID.array_recordOid |]
  let
    fields = map VarT names
    tuple = ConT ''Tuple `AppT` foldl AppT (TupleT n) fields
    context = [ ConT ''FromSqlField `AppT` ty | ty <- fields ]
    width = TySynEqn Nothing (ConT ''Width `AppT` tuple) (LitT (NumTyLit 1))
    tupleSizeE = LitE  (IntegerL (toInteger n))
    parser = VarE 'fmap `AppE` ConE 'Tuple `AppE` (VarE 'composite `AppE` tupleSizeE `AppE` foldl
             (\parser field -> VarE '(<*>) `AppE` parser `AppE` field)
             (VarE 'pure `AppE` ConE (tupleDataName n))
             (replicate n (VarE 'valueComposite `AppE` VarE 'fromSqlField)))
    method = ValD
      (VarP 'fromSqlField)
      (NormalB (ConE 'FieldDecoder `AppE` fieldOid `AppE` parser))
      []
  return [ InstanceD Nothing context (ConT ''FromSqlField `AppT` tuple) [method]
    , InstanceD Nothing context (ConT ''FromSql `AppT` tuple) [ ] ]


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
