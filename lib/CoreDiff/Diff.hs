module CoreDiff.Diff where

import Data.List
import GhcDump.Ast
import qualified Data.Text as T

type StrExpr = Expr' String String
type StrAlt = Alt' String String
type StrType = Type' String String

{-
Convert all binders, variables and external names in an expression to strings.
-}
strExpr :: SExpr -> StrExpr
strExpr (EVar var) = EVar (showBinderId var)
-- TODO: Not perfect
strExpr (EVarGlobal extName) = EVar (showExtName extName)
strExpr (ELit lit) = ELit lit
strExpr (EApp f x) = EApp (strExpr f) (strExpr x)
strExpr (ETyLam p b) = ETyLam (showSBndr p) (strExpr b)
strExpr (ELam p b) = ELam (showSBndr p) (strExpr b)
strExpr (ELet defs expr) = ELet (map go defs) (strExpr expr)
  where go (bndr, expr) = (showSBndr bndr, strExpr expr)
strExpr (ECase match bndr alts) = ECase (strExpr match) (showSBndr bndr) (map strAlt alts)
strExpr (EType ty) = EType (strType ty)
strExpr ECoercion = ECoercion

{-
TODO: Need to change altCon field too?
-}
strAlt :: SAlt -> StrAlt
strAlt alt = alt
  { altBinders = map showSBndr $ altBinders alt
  , altRHS = strExpr $ altRHS alt
  }

strType :: SType -> StrType
strType (VarTy var) = VarTy (showBinderId var)
strType (FunTy lhs rhs) = FunTy (strType lhs) (strType rhs)
strType (TyConApp tyCon args) = TyConApp tyCon (map strType args)
strType (AppTy f x) = AppTy (strType f) (strType x)
strType (ForAllTy bndr ty) = ForAllTy (showSBndr bndr) (strType ty)
strType LitTy = LitTy
strType CoercionTy = CoercionTy

type MetaVar = Int

type ExprChange var = (ExprC var, ExprC var)

data ExprC metavar
  = EVarC String
  | EVarGlobalC String
  | ELitC Lit
  | EAppC (ExprC metavar) (ExprC metavar)
  | ETyLamC String (ExprC metavar)
  | ELamC String (ExprC metavar)
  | ELetC [(String, ExprC metavar)] (ExprC metavar)
  | ECaseC (ExprC metavar) String [AltC metavar]
  | ETypeC (TypeC metavar)
  | ECoercionC
  | ExprHole metavar
  deriving (Show)

data AltC metavar = AltC
  { altCCon :: AltCon
  , altCBinders :: [String]
  , altCRHS :: ExprC metavar
  }
  | AltHole metavar
  deriving (Show)

data TypeC metavar
  = VarTyC String
  | FunTyC (TypeC metavar) (TypeC metavar)
  | TyConAppC TyCon [TypeC metavar]
  | AppTyC (TypeC metavar) (TypeC metavar)
  | ForAllTyC String (TypeC metavar)
  | LitTyC
  | CoercionTyC
  | TypeHole metavar
  deriving (Show)

showSBndr = T.unpack . binderName . unSBndr
-- TODO: look this up in current binder context
showBinderId (BinderId uniq) = show uniq
showExtName e = T.unpack $ getModuleName (externalModuleName e) <> T.pack "." <> externalName e

diffExpr :: StrExpr -> StrExpr -> ExprChange MetaVar
diffExpr = changeExpr

data Oracles metavar = Oracles
  { oracleExpr :: StrExpr -> Maybe metavar
  , oracleType :: StrType -> Maybe metavar
  , oracleAlt :: StrAlt -> Maybe metavar
  }

{-|
  Compute changes between two expressions.
  In order to keep the resulting tuple small, common subexpressions are identified
  and marked as holes that can be copied from the left context to the right.
  The oracle function is used here to assign metavariables to subexpressions.
-}
changeExpr :: StrExpr -> StrExpr -> ExprChange MetaVar
changeExpr a b = (extractExpr oracles a, extractExpr oracles b)
  where oracles = Oracles (wcs a b) (wcsType a b) (wcsAlt a b)

{-|
  Check whether the third expression is a subexpression of both the
  first and the second. If so, it is assigned an unique metavariable among all
  common subexpressions.
-}
wcs :: StrExpr -> StrExpr -> StrExpr -> Maybe MetaVar
wcs a b sub = findIndex (== sub) $ intersect (subExpr a) (subExpr b)

wcsType a b sub =
  findIndex (== sub) $ intersect (concat $ map subType $ types a) (concat $ map subType $ types b)

wcsAlt a b sub =
  findIndex (== sub) $ intersect (alts a) (alts b)

{-|
  Generate all subexpressions for a given expression.
-}
subExpr :: StrExpr -> [StrExpr]
subExpr expr = expr : subExpr' expr
  where
    subExpr' (EApp f x) = subExpr f ++ subExpr x
    subExpr' (ETyLam _ b) = subExpr b
    subExpr' (ELam _ b) = subExpr b
    subExpr' (ELet bindings body) = concat (map (subExpr . snd) bindings) ++ subExpr body
    subExpr' (ECase match _ alts) = subExpr match ++ concat (map (subExpr . altRHS) alts)
    subExpr' _ = []

types :: StrExpr -> [StrType]
types (EApp f x) = types f ++ types x
types (ETyLam _ b) = types b
types (ELam _ b) = types b
types (ELet bindings body) = concat (map (types . snd) bindings) ++ types body
types (ECase match _ alts) = types match ++ concat (map (types . altRHS) alts)
types (EType ty) = [ty]
types _ = []

alts :: StrExpr -> [StrAlt]
alts (EApp f x) = alts f ++ alts x
alts (ETyLam _ b) = alts b
alts (ELam _ b) = alts b
alts (ELet bindings body) = concat (map (alts . snd) bindings) ++ alts body
alts (ECase match _ alts') = alts match ++ alts'
alts _ = []

subType :: StrType -> [StrType]
subType ty = ty : subType' ty
  where
    subType' (FunTy lhs rhs) = subType lhs ++ subType rhs
    subType' (TyConApp _ args) = concat $ map subType args
    subType' (AppTy f x) = subType f ++ subType x
    subType' (ForAllTy _ ty) = subType ty
    subType' _ = []

extractExpr oracles expr = maybe (peel expr) ExprHole (oracleExpr oracles expr)
  where
    peel (EVar v) = EVarC v
    -- peel (EVarGlobal v) = EVarGlobalC v -- Shouldn't be necessary after conversion to StrExpr
    peel (ELit l) = ELitC l
    peel (EApp f x) = EAppC (extractExpr oracles f) (extractExpr oracles x)
    peel (ETyLam p b) = ETyLamC p (extractExpr oracles b)
    peel (ELam p b) = ELamC p (extractExpr oracles b)
    peel (ELet bindings body) = ELetC (mapSnd (extractExpr oracles) bindings) (extractExpr oracles body)
    peel (ECase match bndr alts) = ECaseC (extractExpr oracles match) bndr (map (extractAlt oracles) alts)
    peel (EType ty) = ETypeC (extractType oracles ty)
    peel ECoercion = ECoercionC

extractType oracles ty = maybe (peel ty) TypeHole (oracleType oracles ty)
  where
    peel (VarTy v) = VarTyC v
    peel (FunTy lhs rhs) = FunTyC (extractType oracles lhs) (extractType oracles rhs)
    peel (TyConApp tyCon args) = TyConAppC tyCon (map (extractType oracles) args)
    peel (AppTy f x) = AppTyC (extractType oracles f) (extractType oracles x)
    peel (ForAllTy bndr ty) = ForAllTyC bndr (extractType oracles ty)
    peel LitTy = LitTyC
    peel CoercionTy = CoercionTyC

extractAlt oracles alt = maybe (peel alt) AltHole (oracleAlt oracles alt)
  where
    peel alt = AltC
      { altCCon = altCon alt
      , altCBinders = altBinders alt
      , altCRHS = extractExpr oracles (altRHS alt)
      }

mapSnd f list = [(x, f y) | (x, y) <- list]
