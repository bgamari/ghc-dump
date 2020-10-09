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
import Unsafe.Coerce

data XBinding (a :: Variant)
  = XBinding (XBinder a) (XExpr a)
  | XXBinding (XBindingExtension a)

xBindingBinder (XBinding bndr _) = bndr

data XBinder (a :: Variant)
  = XBinder
    { xBinderName :: T.Text
    , xBinderId :: BinderId
    , xBinderScope :: IdScope
    , xBinderIdInfo :: IdInfo Binder Binder
    , xBinderType :: XType a
    }
  | XTyBinder
    { xBinderName :: T.Text
    , xBinderId :: BinderId
    , xBinderKind :: XType a
    }
  | XXBinder (XBinderExtension a)

-- ExternalName from GhcDump.Ast without the Unique field.
-- Provides a simple derived Eq instance.
-- TODO: field names and type name are less than nice, what
-- are the alternatives here?
data ExternalName' = ExternalName'
  { externalModuleName' :: ModuleName
  , externalName' :: T.Text
  }
  | ForeignCall'
  deriving (Show, Eq, Ord)

data XExpr (a :: Variant)
  = XVar (XBinder a)
  | XVarGlobal ExternalName'
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

-- Getters and setters

data XBinderUniqueName = XBinderUniqueName T.Text BinderId
  deriving (Eq, Ord, Show)

xBinderUName :: XBinder a -> XBinderUniqueName
xBinderUName binder = XBinderUniqueName (xBinderName binder) (xBinderId binder)

xBinderSetUName :: XBinder a -> XBinderUniqueName -> XBinder a
xBinderSetUName binder (XBinderUniqueName name id) =
  binder { xBinderName = name, xBinderId = id }

xBinderTypeOrKind b@XBinder{}   = xBinderType b
xBinderTypeOrKind b@XTyBinder{} = xBinderKind b

-- TyCon from GhcDump.Ast without the Unique field.
-- Provides a simple derived Eq instance.
-- TODO: newtype?
data TyCon' = TyCon' T.Text
  deriving (Show, Eq, Ord)

data XType (a :: Variant)
  = XVarTy (XBinder a)
  | XFunTy (XType a) (XType a)
  | XTyConApp TyCon' [XType a]
  | XAppTy (XType a) (XType a)
  | XForAllTy (XBinder a) (XType a)
  | XLitTy
  | XCoercionTy
  | XXType (XTypeExtension a)

deriving instance ForAllExtensions Show a => Show (XBinding a)
deriving instance ForAllExtensions Show a => Show (XBinder a)
deriving instance ForAllExtensions Show a => Show (XExpr a)
deriving instance ForAllExtensions Show a => Show (XAlt a)
deriving instance ForAllExtensions Show a => Show (XType a)

deriving instance ForAllExtensions Eq a => Eq (XBinding a)
deriving instance ForAllExtensions Eq a => Eq (XExpr a)
deriving instance ForAllExtensions Eq a => Eq (XAlt a)
deriving instance ForAllExtensions Eq a => Eq (XType a)

deriving instance ForAllExtensions Ord a => Ord (XBinding a)
deriving instance ForAllExtensions Ord a => Ord (XExpr a)
deriving instance ForAllExtensions Ord a => Ord (XAlt a)
deriving instance ForAllExtensions Ord a => Ord (XType a)

-- equality on binders is defined as the equality of their name and unique field only
instance ForAllExtensions Eq a => Eq (XBinder a) where
  b == b' = xBinderUName b == xBinderUName b'

instance ForAllExtensions Ord a => Ord (XBinder a) where
  b <= b' = xBinderUName b <= xBinderUName b'

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


-- we can convert any undecorated type into any other variant of the same type.
-- we don't need to convert its extension since the extension constructor
-- can't possibly be instantiated for undecorated types.
-- The solution below feels dirty but works perfectly well.
fromUD :: t UD -> t (a :: Variant)
fromUD = unsafeCoerce


newtype Change a = Change (a, a)

-- functions n stuff

cvtBinding :: (Binder, Expr) -> XBinding UD
cvtBinding (binder, expr) = XBinding (cvtBinder binder) (cvtExpr expr)

cvtBinder :: Binder -> XBinder UD
cvtBinder (Bndr b@Binder{}) = XBinder
  (binderName b)
  (binderId b)
  (binderScope b)
  (removeUnfolding $ binderIdInfo b)
  (cvtType $ binderType b)
  where
    -- TODO: Some people may want to diff for unfoldings.
    -- In that case, we should not remove them here.
    removeUnfolding idi@IdInfo{} =
      idi { idiUnfolding = NoUnfolding }
cvtBinder (Bndr b@TyBinder{}) = XTyBinder
  (binderName b)
  (binderId b)
  (cvtType $ binderKind b)

cvtExpr :: Expr -> XExpr UD
cvtExpr (EVar binder) = XVar $ cvtBinder binder
cvtExpr (EVarGlobal extName) = XVarGlobal $ cvtExtName extName
  where
    cvtExtName (ExternalName modName name _) = ExternalName' modName name
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
cvtType (TyConApp tc args) = XTyConApp (cvtTyCon tc) (map cvtType args)
  where
    cvtTyCon (TyCon name _) = TyCon' name
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

-- signatures
newtype Signature = Signature (T.Text, XType UD)

signature :: XBinder UD -> Signature
signature b@XBinder{}   = Signature (xBinderName b, xBinderType b)
signature b@XTyBinder{} = Signature (xBinderName b, xBinderKind b)
