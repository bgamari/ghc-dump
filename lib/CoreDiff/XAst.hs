{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf
-- TODO: haddock zeug

module CoreDiff.XAst where

import Data.Void (Void)
import GhcDump.Ast

data XBinding (a :: Variant)
  = XBinding (XBinder a) (XExpr a)
  | XXBinding (XBindingExtension a)

data XBinder (a :: Variant)
  = XBinder Binder
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
  | XType Type
  | XCoercion
  | XXExpr (XExprExtension a)

data XAlt (a :: Variant)
  = XAlt
    { xAltCon :: AltCon
    , xAltBinders :: [XBinder a]
    , xAltRHS :: XExpr a
    }
  | XXAlt (XAltExtension a)

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

newtype Change a = Change (a, a)

-- functions n stuff

cvtBinding :: (Binder, Expr) -> XBinding UD
cvtBinding (binder, expr) = XBinding (cvtBinder binder) (cvtExpr expr)

cvtBinder :: Binder -> XBinder UD
cvtBinder binder = XBinder binder

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
cvtExpr (EType ty) = XType ty
cvtExpr (ECoercion) = XCoercion

cvtAlt :: Alt -> XAlt UD
cvtAlt (Alt con binders rhs) = XAlt con (map cvtBinder binders) (cvtExpr rhs)
