 {-# LANGUAGE DataKinds #-}
 {-# LANGUAGE KindSignatures #-}
 {-# LANGUAGE TypeFamilies #-}

module CoreDiff.XAst where

import Data.Text as T
import Data.Void


data XModule = XModule
  { xModuleName :: T.Text
  , xModulePhase :: T.Text
  , xModuleBindings :: [XBinding UD]
  }


data Variant = UD | Diff


data XBinding (a :: Variant)
  = XBinding (XBinder a) (XExpr a)
  | XXBinding (XBindingExt a)

data XBinder (a :: Variant)
  = XXBinder (XBinderExt a)

data XType (a :: Variant)
  = XXType (XTypeExt a)

data XExpr (a :: Variant)
  = XXExpr (XExprExt a)

data XAlt (a :: Variant)
  = XXAlt (XAltExt a)


newtype Change a = Change (a, a)


type family XBindingExt a where
  XBindingExt UD   = Void
  XBindingExt Diff = Change (XBinding UD)

type family XBinderExt a where
  XBinderExt UD   = Void
  XBinderExt Diff = Change (XBinder UD)

type family XTypeExt a where
  XTypeExt UD   = Void
  XTypeExt Diff = Change (XType UD)

type family XExprExt a where
  XExprExt UD   = Void
  XExprExt Diff = Change (XExpr UD)

type family XAltExt a where
  XAltExt UD = Void
  XAltExt Diff = Change (XAlt UD)
