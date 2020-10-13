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
cvtType = error "stub"

cvtExpr :: Expr -> XExpr UD
cvtExpr = error "stub"
