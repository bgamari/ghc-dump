{-# LANGUAGE CPP #-}
module Convert where

import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Var (Var, varUnique, varType, isTyVar)
import Module (moduleNameFS, moduleName)
import Unique (Unique, unpkUnique)
import Name (getOccName, occNameFS, OccName)
import CoreStats (exprStats, CoreStats(..))
import CoreSyn (Expr(..), CoreExpr, Bind(..), CoreAlt, collectArgs)
import HscTypes (ModGuts(..))
import FastString (FastString, fastStringToByteString)
import TyCoRep (Type(..), TyBinder(..))
import Type (splitFunTy_maybe)
import TyCon (TyCon, tyConUnique)

import Ast

fastStringToText :: FastString -> T.Text
fastStringToText = TE.decodeUtf8 . fastStringToByteString

occNameToText :: OccName -> T.Text
occNameToText = fastStringToText . occNameFS

cvtUnique :: Unique.Unique -> Ast.Unique
cvtUnique u =
    let (a,b) = unpkUnique u
    in Ast.Unique a b

cvtVar :: Var -> BinderId
cvtVar = BinderId . cvtUnique . varUnique

cvtBinder :: Var -> Binder
cvtBinder v = Binder (occNameToText $ getOccName v) (cvtVar v)

cvtCoreStats :: CoreStats.CoreStats -> Ast.CoreStats
cvtCoreStats stats =
    Ast.CoreStats
      { cs_terms      = cs_tm stats
      , cs_types      = cs_ty stats
      , cs_coercions  = cs_co stats
#if MIN_VERSION_ghc(8,2,0)
      , cs_val_binds  = cs_vb stats
      , cs_join_binds = cs_jb stats
#else
      , cs_val_binds  = 0
      , cs_join_binds = 0
#endif
      }

cvtTopBind (NonRec b e) = NonRecTopBinding (cvtBinder b) (cvtCoreStats $ exprStats e) (cvtExpr e)
cvtTopBind (Rec bs)     = RecTopBinding $ map to bs
  where to (b, e) = (cvtBinder b, cvtCoreStats $ exprStats e, cvtExpr e)

cvtExpr :: CoreExpr -> Ast.Expr
cvtExpr expr =
  case expr of
    Var x             -> EVar (cvtVar x)
    Lit l             -> ELit
    App {}            -> let (x, ys) = collectArgs expr
                         in EApp (cvtExpr x) (map cvtExpr ys)
    Lam x e
      | isTyVar x     -> ETyLam (cvtBinder x) (cvtExpr e)
      | otherwise     -> ELam (cvtBinder x) (cvtExpr e)
    Let (NonRec b e) body -> ELet [(cvtBinder b, cvtExpr e)] (cvtExpr body)
    Let (Rec bs) body -> ELet (map (bimap cvtBinder cvtExpr) bs) (cvtExpr body)
    Case e x _ as     -> ECase (cvtExpr e) (cvtBinder x) (map cvtAlt as)
    Cast x _          -> cvtExpr x
    Tick _ e          -> cvtExpr e
    Type t            -> EType $ cvtType t
    Coercion _        -> ECoercion

cvtAlt :: CoreAlt -> Ast.Alt
cvtAlt (con, bs, e) = Alt T.empty (map cvtBinder bs) (cvtExpr e)
--(occNameToText $ getOccName con)

cvtModule :: ModGuts -> Ast.Module
cvtModule guts = Ast.Module name (map cvtTopBind $ mg_binds guts)
  where
    name = Ast.ModuleName $ fastStringToText $ moduleNameFS $ moduleName $ mg_module guts

cvtType :: TyCoRep.Type -> Ast.Type
cvtType t
  | Just (a,b) <- splitFunTy_maybe t = Ast.FunTy (cvtType a) (cvtType b)
cvtType (TyCoRep.TyVarTy v)       = Ast.VarTy (cvtVar v)
cvtType (TyCoRep.AppTy a b)       = Ast.AppTy (cvtType a) (cvtType b)
cvtType (TyCoRep.TyConApp tc tys) = Ast.TyConApp (cvtTyCon tc) (map cvtType tys)
cvtType (TyCoRep.ForAllTy b t)    = Ast.ForAllTy (cvtTyBinder b) (cvtType t)
cvtType (TyCoRep.LitTy _)         = Ast.LitTy
cvtType (TyCoRep.CastTy t _)      = cvtType t
cvtType (TyCoRep.CoercionTy _)    = Ast.CoercionTy

cvtTyBinder :: TyCoRep.TyBinder -> Ast.TyBinder
cvtTyBinder (Named v _) = NamedTyBinder (cvtBinder v) (cvtType $ varType v)
cvtTyBinder (Anon t)    = AnonTyBinder (cvtType t)

cvtTyCon :: TyCon.TyCon -> Ast.TyCon
cvtTyCon tc = TyCon (occNameToText $ getOccName tc) (cvtUnique $ tyConUnique tc)
