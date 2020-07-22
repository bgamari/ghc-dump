module CoreDiff.Diff where

import Data.List
import GhcDump.Ast
import qualified Data.Text as T

-- each kind of diff-able structure needs
-- * a corresponding context type
-- * a type of metavariable
data BindingC bindingMv exprMv bndrMv altMv
  = BindingC (BndrC bindingMv exprMv bndrMv altMv) (ExprC bindingMv exprMv bndrMv altMv)
  | BindingHole bindingMv
  deriving (Show)

data ExprC bindingMv exprMv bndrMv altMv
  = EVarC (BndrC bindingMv exprMv bndrMv altMv)
  | EVarGlobalC ExternalName
  | ELitC Lit
  | EAppC (ExprC bindingMv exprMv bndrMv altMv) (ExprC bindingMv exprMv bndrMv altMv)
  | ETyLamC (BndrC bindingMv exprMv bndrMv altMv) (ExprC bindingMv exprMv bndrMv altMv)
  | ELamC (BndrC bindingMv exprMv bndrMv altMv) (ExprC bindingMv exprMv bndrMv altMv)
  | ELetC [BindingC bindingMv exprMv bndrMv altMv] (ExprC bindingMv exprMv bndrMv altMv)
  | ECaseC (ExprC bindingMv exprMv bndrMv altMv) (BndrC bindingMv exprMv bndrMv altMv) [AltC bindingMv exprMv bndrMv altMv]
  | ETypeC Type
  | ECoercionC
  | ExprHole exprMv
  deriving (Show)

data BndrC bindingMv exprMv bndrMv altMv
  = BndrC Binder
  | BndrHole bndrMv
  deriving (Show)

data AltC bindingMv exprMv bndrMv altMv
  = AltC
    { altCCon :: AltCon
    , altCBinders :: [BndrC bindingMv exprMv bndrMv altMv]
    , altCRHS :: ExprC bindingMv exprMv bndrMv altMv
    }
  | AltHole altMv
  deriving (Show)

{-
data TypeC metavar
  = VarTyC (BndrC metavar)
  | FunTyC (TypeC metavar) (TypeC metavar)
  | TyConAppC TyCon [TypeC metavar]
  | AppTyC (TypeC metavar) (TypeC metavar)
  | ForAllTyC (BndrC metavar) (TypeC metavar)
  | LitTyC
  | CoercionTyC
  | TypeHole metavar
  deriving (Show)
-}

newtype Change t = Change (t, t)

type Diff t = t (Change (Binder, Expr)) (Change Expr) (Change Binder) (Change Alt)

-- Calculate spine of two contexts a.k.a. their Greatest Common Prefix
gcpBinding :: (Binder, Expr) -> (Binder, Expr) -> Diff BindingC
gcpBinding (bndr, expr) (bndr', expr') =
  BindingC (gcpBndr bndr bndr') (gcpExpr expr expr')
{-
gcpBinding a b =
  BindingHole $ Change (a, b)
-}

gcpBndr bndr bndr'
  | bndr == bndr' = BndrC bndr 
gcpBndr a b =
  BndrHole $ Change (a, b)

-- This could be a little shorter (grouping EVarC, EVarGlobalC, ELitC and ECoercionC by matching interesting terms first then checking equality only).
-- But for now we're gonna stay explicit.
gcpExpr (EVar var) (EVar var') = EVarC $ gcpBndr var var'
gcpExpr (EVarGlobal extName) (EVarGlobal extName')
  | extName == extName' = EVarGlobalC extName
gcpExpr (ELit lit) (ELit lit')
  | lit == lit' = ELitC lit
gcpExpr (EApp f x) (EApp f' x') =
  EAppC (gcpExpr f f') (gcpExpr x x')
gcpExpr (ETyLam p b) (ETyLam p' b') =
  ETyLamC (gcpBndr p p') (gcpExpr b b')
gcpExpr (ELam p b) (ELam p' b') =
  ELamC (gcpBndr p p') (gcpExpr b b')
gcpExpr (ELet bindings expr) (ELet bindings' expr')
  | length bindings == length bindings' = ELetC (zipWith gcpBinding bindings bindings') (gcpExpr expr expr')
gcpExpr (ECase match bndr alts) (ECase match' bndr' alts')
  | length alts == length alts' =
    ECaseC (gcpExpr match match') (gcpBndr bndr bndr') (zipWith gcpAlt alts alts')
gcpExpr (EType ty) (EType ty')
  | ty == ty' = ETypeC ty
gcpExpr ECoercion ECoercion = ECoercionC
gcpExpr a b = ExprHole $ Change (a, b)

-- TODO
gcpAlt (Alt con bndrs rhs) (Alt con' bndrs' rhs')
  | con == con' = AltC con (zipWith gcpBndr bndrs bndrs') (gcpExpr rhs rhs')
gcpAlt a b = AltHole $ Change (a, b)
