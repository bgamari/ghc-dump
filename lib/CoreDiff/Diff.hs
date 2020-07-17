module CoreDiff.Diff where

import Data.List
import GhcDump.Ast
import qualified Data.Text as T

type MetaVar = Int

-- each kind of diff-able structure needs
-- * a corresponding context type
-- * a type of metavariable
data BindingC bindingMv exprMv bndrMv
  = BindingC (BndrC bindingMv exprMv bndrMv) (ExprC bindingMv exprMv bndrMv)
  | BindingHole bindingMv
  deriving (Show)

data ExprC bindingMv exprMv bndrMv
  = EVarC BinderId
  | EVarGlobalC ExternalName
  | ELitC Lit
  | EAppC (ExprC bindingMv exprMv bndrMv) (ExprC bindingMv exprMv bndrMv)
  | ETyLamC (BndrC bindingMv exprMv bndrMv) (ExprC bindingMv exprMv bndrMv)
  | ELamC (BndrC bindingMv exprMv bndrMv) (ExprC bindingMv exprMv bndrMv)
  | ELetC [BindingC bindingMv exprMv bndrMv] (ExprC bindingMv exprMv bndrMv)
  | ECaseC (ExprC bindingMv exprMv bndrMv) (BndrC bindingMv exprMv bndrMv) [AltC bindingMv exprMv bndrMv]
  | ETypeC SType
  | ECoercionC
  | ExprHole exprMv
  deriving (Show)

data BndrC bindingMv exprMv bndrMv
  = BndrC SBinder
  | BndrHole bndrMv
  deriving (Show)

-- case alternatives are not diff-able by themselves (yet)
data AltC bindingMv exprMv bndrMv = AltC
  { altCCon :: AltCon
  , altCBinders :: [BndrC bindingMv exprMv bndrMv]
  , altCRHS :: ExprC bindingMv exprMv bndrMv
  }
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

newtype ChangeBinding = ChangeBinding (BindingC Int Int Int, BindingC Int Int Int)

data Oracles = Oracles
  { bndrWcs :: SBinder -> Maybe Int
  , bindingWcs :: (SBinder, SExpr) -> Maybe Int
  , exprWcs :: SExpr -> Maybe Int
  }

changeBinding :: (SBinder, SExpr) -> (SBinder, SExpr) -> ChangeBinding
changeBinding bndgA bndgB =
  ChangeBinding (extractBinding o bndgA, extractBinding o bndgB)
  where
    o = oracles bndgA bndgB

-- TODO: rewrite these three using `maybe`
extractBinding :: Oracles -> (SBinder, SExpr) -> BindingC Int Int Int
extractBinding o bndg@(bndr, expr) =
  case bindingWcs o bndg of
    Nothing -> BindingC (extractBndr o bndr) (extractExpr o expr)
    Just hole -> BindingHole hole

extractBndr :: Oracles -> SBinder -> BndrC Int Int Int
extractBndr o bndr =
  case bndrWcs o bndr of
    Nothing -> BndrC bndr
    Just hole -> BndrHole hole

extractExpr :: Oracles -> SExpr -> ExprC Int Int Int
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
    peel (ELet bindings expr) = ELetC (map go bindings) (extractExpr o expr)
      where go (bndr, expr') = BindingC (extractBndr o bndr) (extractExpr o expr') -- TODO: extractBinding
    peel (ECase match bndr alts) = ECaseC (extractExpr o match) (extractBndr o bndr) (map go alts)
      where go alt = AltC (altCon alt) (map (extractBndr o) $ altBinders alt) (extractExpr o $ altRHS alt) -- TODO: extractAlt
    peel (EType t) = ETypeC t
    peel ECoercion = ECoercionC

oracles bndgA bndgB = Oracles
  { bndrWcs = findCommon binders bndgA bndgB
  , bindingWcs = findCommon bindings bndgA bndgB
  , exprWcs = findCommon (exprs . snd) bndgA bndgB
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
