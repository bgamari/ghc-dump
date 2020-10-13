{-# LANGUAGE DataKinds #-}

module CoreDiff.Convert where

import GhcDump.Ast

import CoreDiff.XAst

cvtModule :: Module -> XModule
cvtModule mod = XModule
  { xModuleName = getModuleName $ moduleName mod
  , xModulePhase = modulePhase mod
  , xModuleBindings = map cvtBinding $ map removeStats $ moduleBindings mod
  }
  where removeStats (binder, _stats, expr) = (binder, expr)

cvtBinding :: (Binder, Expr) -> XBinding UD
cvtBinding (binder, expr) = XBinding (cvtBinder binder) (cvtExpr expr)

cvtBinder :: Binder -> XBinder UD
cvtBinder b@(Bndr binder@Binder{}) = XBinder
  (binderName binder)
  (cvtBinderId $ binderId binder)
  (cvtType $ binderType binder)
  (cvtMeta b)
cvtBinder (Bndr binder@TyBinder{}) = XTyBinder
  (binderName binder)
  (cvtBinderId $ binderId binder)
  (cvtType $ binderKind binder)

cvtBinderId (BinderId unique) = unique

cvtMeta :: Binder -> XBinderMeta
cvtMeta (Bndr binder@Binder{}) = XBinderMeta
  { xbmArity = idiArity idi
  , xbmIsOneShot = idiIsOneShot idi
  , xbmInlinePragma = idiInlinePragma idi
  , xbmOccInfo = idiOccInfo idi
  , xbmStrictnessSig = idiStrictnessSig idi
  , xbmDemandSig = idiDemandSig idi
  , xbmCallArity = idiCallArity idi
  , xbmCpr = idiCpr idi
  , xbmScope = binderScope binder
  }
  where
    idi = binderIdInfo binder

cvtType :: Type -> XType UD
cvtType (VarTy binder) = XVarTy $ cvtBinder binder
cvtType (FunTy l r) = XFunTy (cvtType l) (cvtType r)
cvtType (TyConApp tc args) = XTyConApp (cvtTyCon tc) (map cvtType args)
  where cvtTyCon (TyCon name _) = TyCon' name
cvtType (AppTy l r) = XAppTy (cvtType l) (cvtType r)
cvtType (ForAllTy binder ty) = XForAllTy (cvtBinder binder) (cvtType ty)
cvtType (LitTy) = XLitTy
cvtType (CoercionTy) = XCoercionTy

cvtExpr :: Expr -> XExpr UD
cvtExpr (EVar binder) = XVar $ cvtBinder binder
cvtExpr (EVarGlobal extName) = XVarGlobal $ cvtExtName extName
  where cvtExtName (ExternalName modName name _) = ExternalName' (getModuleName modName) name
cvtExpr (ELit lit) = XLit lit
cvtExpr (EApp f x) = XApp (cvtExpr f) (cvtExpr x)
cvtExpr (ETyLam binder expr) = XTyLam (cvtBinder binder) (cvtExpr expr)
cvtExpr (ELam binder expr) = XLam (cvtBinder binder) (cvtExpr expr)
cvtExpr (ELet bindings body) = XLet
  (map cvtBinding bindings)
  (cvtExpr body)
cvtExpr (ECase match binder alts) = XCase
  (cvtExpr match)
  (cvtBinder binder)
  (map cvtAlt alts)
cvtExpr (EType ty) = XType $ cvtType ty
cvtExpr (ECoercion) = XCoercion

cvtAlt :: Alt -> XAlt UD
cvtAlt (Alt con binders rhs) = XAlt con (map cvtBinder binders) (cvtExpr rhs)
