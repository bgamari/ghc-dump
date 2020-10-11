 {-# LANGUAGE DataKinds #-}
 {-# LANGUAGE KindSignatures #-}
 {-# LANGUAGE TypeFamilies #-}

module CoreDiff.XAst where

import Data.Text as T
import Data.Void


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

-- | A Core binder, consisting of a name, unique and some metadata.
data XBinder (a :: Variant)
  = XXBinder (XBinderExt a)

-- | A Core type.
data XType (a :: Variant)
  = XXType (XTypeExt a)

-- | A Core expression.
data XExpr (a :: Variant)
  = XXExpr (XExprExt a)

-- | A Core case alternative.
data XAlt (a :: Variant)
  = XXAlt (XAltExt a)


-- | Newtype for a change.
-- This makes it easy to define a nice show/prettyprint instance for changes.
newtype Change a = Change (a, a)


-- | Extensions for bindings.
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
