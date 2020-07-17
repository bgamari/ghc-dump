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
  = EVarC BinderId
  | EVarGlobalC ExternalName
  | ELitC Lit
  | EAppC (ExprC bindingMv exprMv bndrMv altMv) (ExprC bindingMv exprMv bndrMv altMv)
  | ETyLamC (BndrC bindingMv exprMv bndrMv altMv) (ExprC bindingMv exprMv bndrMv altMv)
  | ELamC (BndrC bindingMv exprMv bndrMv altMv) (ExprC bindingMv exprMv bndrMv altMv)
  | ELetC [BindingC bindingMv exprMv bndrMv altMv] (ExprC bindingMv exprMv bndrMv altMv)
  | ECaseC (ExprC bindingMv exprMv bndrMv altMv) (BndrC bindingMv exprMv bndrMv altMv) [AltC bindingMv exprMv bndrMv altMv]
  | ETypeC SType
  | ECoercionC
  | ExprHole exprMv
  deriving (Show)

data BndrC bindingMv exprMv bndrMv altMv
  = BndrC SBinder
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
  = VarTyC BinderId
  | FunTyC (TypeC metavar) (TypeC metavar)
  | TyConAppC TyCon [TypeC metavar]
  | AppTyC (TypeC metavar) (TypeC metavar)
  | ForAllTyC (BndrC metavar) (TypeC metavar)
  | LitTyC
  | CoercionTyC
  | TypeHole metavar
  deriving (Show)
-}

type Holey t = t Int Int Int Int

newtype Change t = Change (Holey t, Holey t)

type Diff t = t (Change BindingC) (Change ExprC) (Change BndrC) (Change AltC)

data Oracles = Oracles
  { bndrWcs :: SBinder -> Maybe Int
  , bindingWcs :: (SBinder, SExpr) -> Maybe Int
  , exprWcs :: SExpr -> Maybe Int
  , altWcs :: SAlt -> Maybe Int
  }

changeBinding :: (SBinder, SExpr) -> (SBinder, SExpr) -> Change BindingC
changeBinding bndgA bndgB =
  Change (extractBinding o bndgA, extractBinding o bndgB)
  where
    o = oracles bndgA bndgB

-- TODO: rewrite these three using `maybe`
extractBinding :: Oracles -> (SBinder, SExpr) -> Holey BindingC
extractBinding o bndg@(bndr, expr) =
  case bindingWcs o bndg of
    Nothing -> BindingC (extractBndr o bndr) (extractExpr o expr)
    Just hole -> BindingHole hole

extractBndr :: Oracles -> SBinder -> Holey BndrC
extractBndr o bndr =
  case bndrWcs o bndr of
    Nothing -> BndrC bndr
    Just hole -> BndrHole hole

extractExpr :: Oracles -> SExpr -> Holey ExprC
extractExpr o expr =
  case exprWcs o expr of
    Just hole -> ExprHole hole
    Nothing -> peel expr
  where
    peel (EVar v) = EVarC v
    peel (EVarGlobal v) = EVarGlobalC v
    peel (ELit lit) = ELitC lit
    peel (EApp f x) = EAppC (extractExpr o f) (extractExpr o x)
    peel (ETyLam p b) = ETyLamC (extractBndr o p) (extractExpr o b)
    peel (ELam p b) = ELamC (extractBndr o p) (extractExpr o b)
    peel (ELet bindings expr) = ELetC (map (extractBinding o) bindings) (extractExpr o expr)
    peel (ECase match bndr alts) = ECaseC (extractExpr o match) (extractBndr o bndr) (map (extractAlt o) alts)
    peel (EType t) = ETypeC t
    peel ECoercion = ECoercionC

extractAlt :: Oracles -> SAlt -> Holey AltC
extractAlt o alt@(Alt con bndrs rhs) =
  case altWcs o alt of
    Just hole -> AltHole hole
    Nothing -> AltC
      con
      (map (extractBndr o) bndrs)
      (extractExpr o rhs)

oracles bndgA bndgB = Oracles
  { bndrWcs = findCommon binders bndgA bndgB
  , bindingWcs = findCommon bindings bndgA bndgB
  , exprWcs = findCommon (exprs . snd) bndgA bndgB
  , altWcs = findCommon alts bndgA bndgB
  }
  where
    findCommon p lhs rhs s =
      findIndex (== s) $ intersect (p lhs) (p rhs)

    binders (bndr, expr) = bndr : concatMap go (exprs expr)
      where go (ETyLam bndr _)         = [bndr]
            go (ELam bndr _)           = [bndr]
            go (ELet bindings _)       = map fst bindings
            go (ECase _ caseBndr alts) = caseBndr : concatMap altBinders alts
            go _                       = []

    bindings bndg@(_, expr) = [bndg] ++ concatMap go (exprs expr)
      where go (ELet bindings _) = bindings
            go _                 = []

    exprs e = [e] ++ subExprs e
      where subExprs (EApp f x)           = exprs f ++ exprs x
            subExprs (ETyLam _ b)         = exprs b
            subExprs (ELam _ b)           = exprs b
            subExprs (ELet bindings expr) = concatMap (exprs . snd) bindings ++ exprs expr
            subExprs (ECase match _ alts) = exprs match ++ concatMap (exprs . altRHS) alts
            subExprs _                    = []

    alts (_, expr) = concatMap go (exprs expr)
      where go (ECase _ _ alts) = alts
            go _                = []

-- Calculate spine of two contexts a.k.a. their Greatest Common Prefix
gcpBinding :: Holey BindingC -> Holey BindingC -> Diff BindingC
gcpBinding (BindingC bndr expr) (BindingC bndr' expr') =
  BindingC (gcpBndr bndr bndr') (gcpExpr expr expr')
gcpBinding a b =
  BindingHole $ Change (a, b)

gcpBndr (BndrC bndr) (BndrC bndr')
  | bndr == bndr' = BndrC bndr 
gcpBndr a b =
  BndrHole $ Change (a, b)

-- This could be a little shorter (grouping EVarC, EVarGlobalC, ELitC and ECoercionC by matching interesting terms first then checking equality only).
-- But for now we're gonna stay explicit.
gcpExpr (EVarC var) (EVarC var')
  | var == var' = EVarC var
gcpExpr (EVarGlobalC extName) (EVarGlobalC extName')
  | extName == extName' = EVarGlobalC extName
gcpExpr (ELitC lit) (ELitC lit')
  | lit == lit' = ELitC lit
gcpExpr (EAppC f x) (EAppC f' x') =
  EAppC (gcpExpr f f') (gcpExpr x x')
gcpExpr (ETyLamC p b) (ETyLamC p' b') =
  ETyLamC (gcpBndr p p') (gcpExpr b b')
gcpExpr (ELamC p b) (ELamC p' b') =
  ELamC (gcpBndr p p') (gcpExpr b b')
gcpExpr (ELetC bindings expr) (ELetC bindings' expr')
  | length bindings == length bindings' = ELetC (zipWith gcpBinding bindings bindings') (gcpExpr expr expr')
gcpExpr (ECaseC match bndr alts) (ECaseC match' bndr' alts')
  | length alts == length alts' =
    ECaseC (gcpExpr match match') (gcpBndr bndr bndr') (zipWith gcpAlt alts alts')
gcpExpr (ETypeC ty) (ETypeC ty')
  | ty == ty' = ETypeC ty
gcpExpr ECoercionC ECoercionC = ECoercionC
gcpExpr a b = ExprHole $ Change (a, b)

-- TODO
gcpAlt (AltC con bndrs rhs) (AltC con' bndrs' rhs')
  | con == con' = AltC con (zipWith gcpBndr bndrs bndrs') (gcpExpr rhs rhs')
gcpAlt a b = AltHole $ Change (a, b)
