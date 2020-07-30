{-# LANGUAGE DataKinds #-}

module CoreDiff.Diff where

import CoreDiff.XAst
-- TODO: Sometimes we want to convert an XExpr UD to an XExpr Diff
-- Since XExpr UD never has any extensions, this is a no-op
-- The type system can't unify their types tho.
import Unsafe.Coerce

-- Calculate spine of two contexts a.k.a. their Greatest Common Prefix

gcpBinding :: XBinding UD -> XBinding UD -> XBinding Diff
gcpBinding (XBinding binder expr) (XBinding binder' expr') =
  XBinding (gcpBinder binder binder') (gcpExpr expr expr')


gcpBinder :: XBinder UD -> XBinder UD -> XBinder Diff
gcpBinder binder@XBinder{} binder'@XBinder{}
  | binder == binder' = unsafeCoerce binder
gcpBinder binder@XTyBinder{} binder'@XTyBinder{}
  | binder == binder' = unsafeCoerce binder
gcpBinder binder binder' =
  XXBinder $ Change (binder, binder')


gcpExpr :: XExpr UD -> XExpr UD -> XExpr Diff
gcpExpr (XVar binder) (XVar binder') =
  XVar $ gcpBinder binder binder'
gcpExpr (XVarGlobal extName) (XVarGlobal extName') | extName == extName' =
  XVarGlobal extName
gcpExpr (XLit lit) (XLit lit') | lit == lit' =
  XLit lit
gcpExpr (XApp f x) (XApp f' x') =
  XApp (gcpExpr f f') (gcpExpr x x')
gcpExpr (XTyLam p b) (XTyLam p' b') =
  XTyLam (gcpBinder p p') (gcpExpr b b')
gcpExpr (XLam p b) (XLam p' b') =
  XLam (gcpBinder p p') (gcpExpr b b')
gcpExpr (XLet bindings expr) (XLet bindings' expr') | length bindings == length bindings' =
  XLet (zipWith gcpBinding bindings bindings') (gcpExpr expr expr')
gcpExpr (XCase match binder alts) (XCase match' binder' alts') | length alts == length alts' =
  XCase (gcpExpr match match') (gcpBinder binder binder') (zipWith gcpAlt alts alts')
gcpExpr (XCoercion) (XCoercion) =
  XCoercion
gcpExpr (XType ty) (XType ty') =
  XType $ gcpType ty ty'
gcpExpr expr expr' =
  XXExpr $ Change (expr, expr')


gcpAlt :: XAlt UD -> XAlt UD -> XAlt Diff
gcpAlt (XAlt con binders rhs) (XAlt con' binders' rhs') | con == con' =
  XAlt con (zipWith gcpBinder binders binders') (gcpExpr rhs rhs')
gcpAlt alt alt' =
  XXAlt $ Change (alt, alt')


gcpType :: XType UD -> XType UD -> XType Diff
gcpType (XVarTy binder) (XVarTy binder') =
  XVarTy $ gcpBinder binder binder'
gcpType (XFunTy l r) (XFunTy l' r') =
  XFunTy (gcpType l l') (gcpType r r')
gcpType (XTyConApp tc args) (XTyConApp tc' args') | tc == tc' =
  XTyConApp tc $ zipWith gcpType args args'
gcpType (XAppTy f x) (XAppTy f' x') =
  XAppTy (gcpType f f') (gcpType x x')
gcpType (XForAllTy binder ty) (XForAllTy binder' ty') =
  XForAllTy (gcpBinder binder binder') (gcpType ty ty')
gcpType (XLitTy) (XLitTy) =
  XLitTy
gcpType (XCoercionTy) (XCoercionTy) =
  XCoercionTy
gcpType ty ty' =
  XXType $ Change (ty, ty')
