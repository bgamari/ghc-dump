 {-# LANGUAGE ConstraintKinds #-}
 {-# LANGUAGE DataKinds #-}
 {-# LANGUAGE FlexibleContexts #-}
 {-# LANGUAGE KindSignatures #-}
 {-# LANGUAGE StandaloneDeriving #-}
 {-# LANGUAGE TypeFamilies #-}
 {-# LANGUAGE UndecidableInstances #-}

module CoreDiff.XAst where

import Data.Kind (Constraint)
import Data.Text as T
import Data.Void
import GhcDump.Ast


-- | A Core module.
data XModule = XModule
  { xModuleName :: T.Text
  , xModulePhase :: T.Text
  , xModuleBindings :: [XBinding UD]
  }

-- | The data types below can each either be undecorated (UD) or the spine of a diff.
-- The technique used to achieve this is described in <https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf>.
-- Each data type has an extension constructor that can be filled depending on its variant.
data Variant = UD | Diff


-- | A Core binding in a module or a (recursive) let expression.
data XBinding (a :: Variant)
  = XBinding (XBinder a) (XExpr a)
  | XXBinding (XBindingExt a)

-- | A Core binder, consisting of a name, an unique and a type/kind.
-- Term binders also have some metadata.
data XBinder (a :: Variant)
  = XBinder -- ^ For term variable binders.
    { xBinderName :: T.Text
    , xBinderId   :: Unique
    , xBinderType :: XType a
    , xBinderMeta :: XBinderMeta
    }
  | XTyBinder -- ^ For type variable binders.
    { xBinderName :: T.Text
    , xBinderId   :: Unique
    , xBinderKind :: XType a
    }
  | XXBinder (XBinderExt a)

-- | Map a binder to its type or kind.
xBinderTypeOrKind binder@XBinder{}   = xBinderType binder
xBinderTypeOrKind binder@XTyBinder{} = xBinderKind binder

-- | Check whether a binder is marked as exported.
xBinderIsExported binder@XBinder{} =
  xbmScope (xBinderMeta binder) `elem` [GlobalId, LocalIdX]
xBinderIsExported _ = False

-- | This is only used for name permutations.
data XBinderUniqueName = XBinderUniqueName T.Text Unique
  deriving (Eq)

-- | Retrieve a binder's name and unique id.
xBinderUniqueName binder =
  XBinderUniqueName (xBinderName binder) (xBinderId binder)

-- | Override a binder's name and unique id.
xBinderSetUniqueName (XBinderUniqueName name id) binder =
  binder { xBinderName = name, xBinderId = id }

-- | A Core type.
data XType (a :: Variant)
  = XVarTy (XBinder a)
  | XFunTy (XType a) (XType a)
  | XTyConApp TyCon' [XType a]
  | XAppTy (XType a) (XType a)
  | XForAllTy (XBinder a) (XType a)
  | XLitTy
  | XCoercionTy
  | XXType (XTypeExt a)

-- | A Core expression.
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
  | XXExpr (XExprExt a)

-- | A Core case alternative.
data XAlt (a :: Variant)
  = XAlt
  { xAltCon     :: AltCon
  , xAltBinders :: [XBinder a]
  , xAltRHS     :: XExpr a
  }
  | XXAlt (XAltExt a)


-- | Newtype for a change.
-- This makes it easy to define a nice show/prettyprint instance for changes.
newtype Change a = Change (a, a)


-- | Extensions for bindings.
-- Undecorated bindings can never contain an extension constructor.
-- Diff bindings can contain changes.
-- This works the same way for the other data types.
type family XBindingExt a where
  XBindingExt UD   = Void
  XBindingExt Diff = Change (XBinding UD)

-- | Extensions for binders.
type family XBinderExt a where
  XBinderExt UD   = Void
  XBinderExt Diff = Change (XBinder UD)

-- | Extensions for types.
type family XTypeExt a where
  XTypeExt UD   = Void
  XTypeExt Diff = Change (XType UD)

-- | Extensions for expressions.
type family XExprExt a where
  XExprExt UD   = Void
  XExprExt Diff = Change (XExpr UD)

-- | Extensions for case alternatives.
type family XAltExt a where
  XAltExt UD = Void
  XAltExt Diff = Change (XAlt UD)


-- | This constraint makes writing instances a little shorter.
-- Instead of writing (PprOpts (XBindingExtension a), PprOpts (XBinderExtension a), etc. we can just write ForAllExtensions PprOpts a.
type ForAllExtensions (constr :: * -> Constraint) a =
  ( constr (XBindingExt a)
  , constr (XBinderExt a)
  , constr (XTypeExt a)
  , constr (XExprExt a)
  , constr (XAltExt a)
  )


-- | Binder metadata.
data XBinderMeta = XBinderMeta
  { xbmArity         :: Int
  , xbmIsOneShot     :: Bool
  , xbmInlinePragma  :: T.Text
  , xbmOccInfo       :: OccInfo
  , xbmStrictnessSig :: T.Text
  , xbmDemandSig     :: T.Text
  , xbmCallArity     :: Int
  , xbmCpr           :: T.Text
  , xbmScope         :: IdScope
  }
  deriving (Eq, Ord)

-- | Type constructor.
-- @GhcDump.Ast.TyCon@ without the unique field.
data TyCon' = TyCon' T.Text
  deriving (Eq, Ord)

-- | A name referring to something in another module.
-- @GhcDump.Ast.ExternalName@ without the unique field.
data ExternalName' = ExternalName'
  { externalModuleName :: T.Text
  , externalName :: T.Text
  }
  | ForeignCall'
  deriving (Eq, Ord)


deriving instance ForAllExtensions Eq a => Eq (XBinding a)
instance ForAllExtensions Eq a => Eq (XBinder a) where
  binder == binder' = xBinderId binder == xBinderId binder'
deriving instance ForAllExtensions Eq a => Eq (XType a)
deriving instance ForAllExtensions Eq a => Eq (XExpr a)
deriving instance ForAllExtensions Eq a => Eq (XAlt a)

deriving instance ForAllExtensions Ord a => Ord (XBinding a)
instance ForAllExtensions Ord a => Ord (XBinder a) where
  binder <= binder' = xBinderId binder <= xBinderId binder'
deriving instance ForAllExtensions Ord a => Ord (XType a)
deriving instance ForAllExtensions Ord a => Ord (XExpr a)
deriving instance ForAllExtensions Ord a => Ord (XAlt a)
