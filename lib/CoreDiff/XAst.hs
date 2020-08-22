{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf
-- TODO: haddock zeug

module CoreDiff.XAst where

import qualified Data.Text as T
import Data.Kind (Constraint)
import Data.Void (Void)
import GhcDump.Ast

data XBinding (a :: Variant)
  = XBinding (XBinder a) (XExpr a)
  | XXBinding (XBindingExtension a)

data XBinder (a :: Variant)
  = XBinder
    { xBinderName :: T.Text
    , xBinderId :: BinderId
    , xBinderIdInfo :: IdInfo Binder Binder
    , xBinderType :: XType a
    }
  | XTyBinder
    { xBinderName :: T.Text
    , xBinderId :: BinderId
    , xBinderKind :: XType a
    }
  | XXBinder (XBinderExtension a)

data XExpr (a :: Variant)
  = XVar (XBinder a)
  | XVarGlobal ExternalName
  | XLit Lit
  | XApp (XExpr a) (XExpr a)
  | XTyLam (XBinder a) (XExpr a)
  | XLam (XBinder a) (XExpr a)
  | XLet [XBinding a] (XExpr a)
  | XCase (XExpr a) (XBinder a) [XAlt a]
  | XType (XType a)
  | XCoercion
  | XXExpr (XExprExtension a)

data XAlt (a :: Variant)
  = XAlt
    { xAltCon :: AltCon
    , xAltBinders :: [XBinder a]
    , xAltRHS :: XExpr a
    }
  | XXAlt (XAltExtension a)

data XType (a :: Variant)
  = XVarTy (XBinder a)
  | XFunTy (XType a) (XType a)
  | XTyConApp TyCon [XType a]
  | XAppTy (XType a) (XType a)
  | XForAllTy (XBinder a) (XType a)
  | XLitTy
  | XCoercionTy
  | XXType (XTypeExtension a)

deriving instance ForAllExtensions Eq a => Eq (XBinding a)
deriving instance ForAllExtensions Eq a => Eq (XBinder a)
deriving instance ForAllExtensions Eq a => Eq (XExpr a)
-- TODO: Provide out own instance, for example for XExtName's ExternalName, don't compare uniques
deriving instance ForAllExtensions Eq a => Eq (XAlt a)
deriving instance ForAllExtensions Eq a => Eq (XType a)

deriving instance ForAllExtensions Ord a => Ord (XBinding a)
deriving instance ForAllExtensions Ord a => Ord (XBinder a)
deriving instance ForAllExtensions Ord a => Ord (XExpr a)
deriving instance ForAllExtensions Ord a => Ord (XAlt a)
deriving instance ForAllExtensions Ord a => Ord (XType a)

-- UD: Undecorated, "normal" expression without extension.
-- Diff: Additional constructor for (Expr, Expr) holes.
data Variant = UD | Diff

type family XBindingExtension a where
  XBindingExtension UD = Void
  XBindingExtension Diff = Change (XBinding UD)

type family XBinderExtension a where
  XBinderExtension UD = Void
  XBinderExtension Diff = Change (XBinder UD)

type family XExprExtension a where
  XExprExtension UD = Void
  XExprExtension Diff = Change (XExpr UD)

type family XAltExtension a where
  XAltExtension UD = Void
  XAltExtension Diff = Change (XAlt UD)

type family XTypeExtension a where
  XTypeExtension UD = Void
  XTypeExtension Diff = Change (XType UD)


-- makes signatures of instances a little shorter
type ForAllExtensions (constr :: * -> Constraint) a =
  ( constr (XBindingExtension a)
  , constr (XBinderExtension a)
  , constr (XExprExtension a)
  , constr (XAltExtension a)
  , constr (XTypeExtension a)
  )


newtype Change a = Change (a, a)

-- functions n stuff

cvtBinding :: (Binder, Expr) -> XBinding UD
cvtBinding (binder, expr) = XBinding (cvtBinder binder) (cvtExpr expr)

cvtBinder :: Binder -> XBinder UD
cvtBinder (Bndr b@Binder{}) = XBinder
  (binderName b)
  (binderId b)
  (removeUnfolding $ binderIdInfo b) -- TODO: this should be temporary
  (cvtType $ binderType b)
cvtBinder (Bndr b@TyBinder{}) = XTyBinder
  (binderName b)
  (binderId b)
  (cvtType $ binderKind b)

removeUnfolding idi@IdInfo{} =
  idi { idiUnfolding = NoUnfolding }

cvtExpr :: Expr -> XExpr UD
cvtExpr (EVar binder) = XVar $ cvtBinder binder
cvtExpr (EVarGlobal extName) = XVarGlobal extName
cvtExpr (ELit lit) = XLit lit
cvtExpr (EApp f x) = XApp (cvtExpr f) (cvtExpr x)
cvtExpr (ETyLam p b) = XTyLam (cvtBinder p) (cvtExpr b)
cvtExpr (ELam p b) = XLam (cvtBinder p) (cvtExpr b)
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

cvtType :: Type -> XType UD
cvtType (VarTy binder) = XVarTy $ cvtBinder binder
cvtType (FunTy l r) = XFunTy (cvtType l) (cvtType r)
cvtType (TyConApp tc args) = XTyConApp tc $ map cvtType args
cvtType (AppTy l r) = XAppTy (cvtType l) (cvtType r)
cvtType (ForAllTy binder ty) = XForAllTy (cvtBinder binder) (cvtType ty)
cvtType (LitTy) = XLitTy
cvtType (CoercionTy) = XCoercionTy

-- helper functions

xBinderUniqueName :: XBinder a -> T.Text
xBinderUniqueName binder =
  xBinderName binder <> "_" <> T.pack (show uniq)
  where
    BinderId uniq = xBinderId binder
