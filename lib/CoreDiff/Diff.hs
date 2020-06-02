module CoreDiff.Diff where

import Data.List
import GhcDump.Ast
import qualified Data.Text as T

type MetaVar = Int
type CorePatch = CoreChange (CoreChange MetaVar)

type CoreChange var = (CoreC var, CoreC var)

data CoreC metavar
  = EVarC BinderId
  | EVarGlobalC ExternalName
  | ELitC Lit
  | EAppC (CoreC metavar) (CoreC metavar)
  | ETyLamC SBinder (CoreC metavar)
  | ELamC SBinder (CoreC metavar)
  | ELetC [(SBinder, CoreC metavar)] (CoreC metavar)
  | ECaseC (CoreC metavar) SBinder [AltC metavar]
-- | EType (TypeC metavar)
  | ETypeC SType
  | ECoercionC
  | CoreHole metavar

data AltC metavar = AltC
  { altCCon :: AltCon
  , altCBinders :: [SBinder]
  , altCRHS :: CoreC metavar
  }

data TypeC metavar
  = VarTyC BinderId
  | FunTyC (TypeC metavar)
  | TyConAppC TyCon [TypeC metavar]
  | AppTyC (TypeC metavar) (TypeC metavar)
  | ForAllTyC SBinder (TypeC metavar)
  | LitTyC
  | CoercionTyC
  | TypeHole metavar

instance Show mv => Show (CoreC mv) where
  show = showInCtx CtxGlobal

data Ctx = CtxGlobal | CtxAppF | CtxAppX | CtxLamB | CtxLetBody | CtxLetRhs | CtxCaseMatch | CtxCaseRhs deriving Eq

showInCtx _ (EVarC v) =
  show v
showInCtx _ (EVarGlobalC v) =
  showExtName v
showInCtx _ (ELitC l) =
  show l
showInCtx c (EAppC f x) =
  parensIf (c `elem` [CtxAppX]) $ showInCtx CtxAppF f ++ " " ++ showInCtx CtxAppX x
showInCtx c (ETyLamC p b) =
  parensIf (c `elem` [CtxAppX]) $ "P" ++ showSBndr p ++ " -> " ++ showInCtx CtxLamB b
showInCtx c (ELamC p b) =
  parensIf (c `elem` [CtxAppX]) $ "\\" ++ showSBndr p ++ " -> " ++ show b
showInCtx _ (ELetC bindings body) =
  "let " ++ intercalate "; " (map showBinding bindings) ++ " in " ++ showInCtx CtxLetBody body
  where showBinding (bndr, expr) = showSBndr bndr ++ " = " ++ showInCtx CtxLetRhs expr
showInCtx _ (ECaseC match _bndr alts) = "case " ++ showInCtx CtxCaseMatch match ++ " of " ++ intercalate ";" (map showAlt alts)
  where showAlt alt = show (altCCon alt) ++ " -> " ++ showInCtx CtxCaseRhs (altCRHS alt)
showInCtx c (ETypeC t) = parensIf (c `elem` [CtxAppX]) $ showType t
showInCtx _ (ECoercionC) = "CO"
showInCtx _ (CoreHole mv) = "HOLE#(" ++ show mv ++ ")"

showType :: SType -> String
showType (VarTy v) = show v
showType (FunTy a b) = "(" ++ showType a ++ " -> " ++ showType b ++ ")"
showType (TyConApp (TyCon t _) args) = T.unpack t ++ (if length args == 0 then "" else " ") ++ intercalate " " (map showType args)
showType (AppTy f x) = "(" ++ showType f ++ " " ++ showType x ++ ")"
showType (ForAllTy b t) = "forall " ++ show b ++ "." ++ showType t
showType LitTy = "LITTY"
showType CoercionTy = "COTY"

parensIf p str
  | p = "(" ++ str ++ ")"
  | otherwise = str

showSBndr = T.unpack . binderName . unSBndr
showExtName e = T.unpack $ getModuleName (externalModuleName e) <> T.pack "." <> externalName e

diffCore :: SExpr -> SExpr -> CoreChange MetaVar
diffCore = changeCore

{-|
  Compute changes between two expressions.
  In order to keep the resulting tuple small, common subexpressions are identified
  and marked as holes that can be copied from the left context to the right.
  The oracle function is used here to assign metavariables to subexpressions.
-}
changeCore :: SExpr -> SExpr -> CoreChange MetaVar
changeCore a b = (extract oracle a, extract oracle b)
  where oracle = wcs a b

{-|
  Check whether the third expression is a subexpression of both the
  first and the second. If so, it is assigned an unique metavariable among all
  common subexpressions.
-}
wcs :: SExpr -> SExpr -> SExpr -> Maybe MetaVar
wcs a b sub = findIndex (== sub) $ intersect (subExpr a) (subExpr b)

{-|
  Generate all subexpressions for a given expression.
-}
subExpr :: SExpr -> [SExpr]
subExpr expr = expr : subExpr' expr
  where
    subExpr' (EApp f x) = subExpr f ++ subExpr x
    subExpr' (ETyLam _ b) = subExpr b
    subExpr' (ELam _ b) = subExpr b
    subExpr' (ELet bindings body) = concat (map (subExpr . snd) bindings) ++ subExpr body
    subExpr' (ECase match _ alts) = subExpr match ++ concat (map (subExpr . altRHS) alts)
    subExpr' _ = []

extract getMv expr = maybe (peel expr) CoreHole (getMv expr)
  where
    peel (EVar v) = EVarC v
    peel (EVarGlobal v) = EVarGlobalC v
    peel (ELit l) = ELitC l
    peel (EApp f x) = EAppC (extract getMv f) (extract getMv x)
    peel (ETyLam p b) = ETyLamC p (extract getMv b)
    peel (ELam p b) = ELamC p (extract getMv b)
    peel (ELet bindings body) = ELetC (mapSnd (extract getMv) bindings) (extract getMv body)
    peel (ECase match bndr alts) = ECaseC (extract getMv match) bndr (map go alts)
      where go alt = AltC (altCon alt) (altBinders alt) (extract getMv $ altRHS alt)
    peel (EType t) = ETypeC t

mapSnd f list = [(x, f y) | (x, y) <- list]
