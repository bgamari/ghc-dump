{-# LANGUAGE CPP #-}
module GhcDump.Convert where

import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Var (Var, varUnique, varType, isTyVar)
import qualified Var
import Module (ModuleName, moduleNameFS, moduleName)
import Unique (Unique, getUnique, unpkUnique)
import Name (getOccName, occNameFS, OccName, getName, nameModule_maybe)
#if MIN_VERSION_ghc(8,0,0)
import qualified CoreStats
#else
import qualified CoreUtils as CoreStats
#endif
import CoreSyn (Expr(..), CoreExpr, Bind(..), CoreAlt, CoreBind, collectArgs)
import HscTypes (ModGuts(..))
import FastString (FastString, fastStringToByteString)
#if MIN_VERSION_ghc(8,0,0)
import TyCoRep as Type (Type(..), TyBinder(..))
#else
import TypeRep as Type (Type(..))
#endif
import Type (splitFunTy_maybe)
import TyCon (TyCon, tyConUnique)

import GhcDump.Ast as Ast

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

cvtBinder :: Var -> SBinder
cvtBinder v = SBndr $ Binder (occNameToText $ getOccName v) (cvtVar v) (cvtType $ varType v)

cvtCoreStats :: CoreStats.CoreStats -> Ast.CoreStats
cvtCoreStats stats =
    Ast.CoreStats
      { cs_terms      = CoreStats.cs_tm stats
      , cs_types      = CoreStats.cs_ty stats
      , cs_coercions  = CoreStats.cs_co stats
#if MIN_VERSION_ghc(8,2,0)
      , cs_val_binds  = CoreStats.cs_vb stats
      , cs_join_binds = CoreStats.cs_jb stats
#else
      , cs_val_binds  = 0
      , cs_join_binds = 0
#endif
      }

exprStats :: CoreExpr -> CoreStats.CoreStats
#if MIN_VERSION_ghc(8,0,0)
exprStats = CoreStats.exprStats
#else
-- exprStats wasn't exported in 7.10
exprStats expr = CoreStats.CS 0 0 0
#endif

cvtTopBind :: CoreBind -> STopBinding
cvtTopBind (NonRec b e) =
    NonRecTopBinding (cvtBinder b) (cvtCoreStats $ exprStats e) (cvtExpr e)
cvtTopBind (Rec bs) =
    RecTopBinding $ map to bs
  where to (b, e) = (cvtBinder b, cvtCoreStats $ exprStats e, cvtExpr e)

cvtExpr :: CoreExpr -> Ast.SExpr
cvtExpr expr =
  case expr of
    Var x
      | Just mod <- nameModule_maybe $ getName x
                      -> EVarGlobal $ ExternalName (cvtModuleName $ Module.moduleName mod)
                                                   (occNameToText $ getOccName x)
                                                   (cvtUnique $ getUnique x)
      | otherwise     -> EVar (cvtVar x)
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

cvtAlt :: CoreAlt -> Ast.SAlt
cvtAlt (con, bs, e) = Alt T.empty (map cvtBinder bs) (cvtExpr e)
--(occNameToText $ getOccName con)

cvtModule :: ModGuts -> Ast.SModule
cvtModule guts = Ast.Module name (map cvtTopBind $ mg_binds guts)
  where name = cvtModuleName $ Module.moduleName $ mg_module guts

cvtModuleName :: Module.ModuleName -> Ast.ModuleName
cvtModuleName = Ast.ModuleName . fastStringToText . moduleNameFS

cvtType :: Type.Type -> Ast.SType
cvtType t
  | Just (a,b) <- splitFunTy_maybe t = Ast.FunTy (cvtType a) (cvtType b)
cvtType (Type.TyVarTy v)       = Ast.VarTy (cvtVar v)
cvtType (Type.AppTy a b)       = Ast.AppTy (cvtType a) (cvtType b)
cvtType (Type.TyConApp tc tys) = Ast.TyConApp (cvtTyCon tc) (map cvtType tys)
#if MIN_VERSION_ghc(8,2,0)
cvtType (Type.ForAllTy (Var.TvBndr b _) t) = Ast.ForAllTy (cvtBinder b) (cvtType t)
#elif MIN_VERSION_ghc(8,0,0)
cvtType (Type.ForAllTy (Named b _) t) = Ast.ForAllTy (cvtBinder b) (cvtType t)
cvtType (Type.ForAllTy (Anon _) t)    = cvtType t
#else
cvtType (Type.ForAllTy b t) = Ast.ForAllTy (cvtBinder b) (cvtType t)
#endif
cvtType (Type.LitTy _)         = Ast.LitTy
#if MIN_VERSION_ghc(8,0,0)
cvtType (Type.CastTy t _)      = cvtType t
cvtType (Type.CoercionTy _)    = Ast.CoercionTy
#endif

cvtTyCon :: TyCon.TyCon -> Ast.TyCon
cvtTyCon tc = TyCon (occNameToText $ getOccName tc) (cvtUnique $ tyConUnique tc)
