module CoreDiff.Diff where

import Data.List
import GhcDump.Ast
import qualified Data.Text as T

type MetaVar = Int

data BindingC metavar
  = BindingC (BndrC metavar) (ExprC metavar)
  deriving (Show)

data ExprC metavar
  = EVarC BinderId
  | EVarGlobalC ExternalName
  | ELitC Lit
  | EAppC (ExprC metavar) (ExprC metavar)
  | ETyLamC (BndrC metavar) (ExprC metavar)
  | ELamC (BndrC metavar) (ExprC metavar)
  | ELetC [BindingC metavar] (ExprC metavar)
  | ECaseC (ExprC metavar) (BndrC metavar) [AltC metavar]
  | ETypeC SType
  | ECoercionC
  | ExprHole metavar
  deriving (Show)

data AltC metavar = AltC
  { altCCon :: AltCon
  , altCBinders :: [BndrC metavar]
  , altCRHS :: ExprC metavar
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

data Oracles = Oracles
  { bndrWcs :: SBinder -> Maybe Int
  , exprWcs :: SExpr -> Maybe Int
  }

data BndrC metavar
  = BndrC SBinder
  | BndrHole metavar
  deriving (Show)

changeBinding :: (SBinder, SExpr) -> (SBinder, SExpr) -> (BindingC Int, BindingC Int)
changeBinding bndgA@(bndrA, exprA) bndgB@(bndrB, exprB) =
  ( BindingC (extractBndr o bndrA) (extractExpr o exprA)
  , BindingC (extractBndr o bndrB) (extractExpr o exprB)
  )
  where
    o = oracles bndgA bndgB

extractBndr :: Oracles -> SBinder -> BndrC Int
extractBndr o bndr =
  case bndrWcs o bndr of
    Nothing -> BndrC bndr
    Just hole -> BndrHole hole

extractExpr :: Oracles -> SExpr -> ExprC Int
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
  { bndrWcs = \bndr -> findIndex (== bndr) $ intersect (binders bndgA) (binders bndgB)
  , exprWcs = \expr -> findIndex (== expr) $ intersect (exprs $ snd bndgA) (exprs $ snd bndgB)
  }
  where
    binders (bndr, expr) = [bndr] ++ concatMap go (exprs expr)
      where go (ETyLam bndr _)         = [bndr]
            go (ELam bndr _)           = [bndr]
            go (ELet bindings _)       = map fst bindings
            go (ECase _ caseBndr alts) = [caseBndr] ++ concatMap altBinders alts
            go _                       = []

    exprs e = [e] ++ subExprs e
      where subExprs (EApp f x)           = exprs f ++ exprs x
            subExprs (ETyLam _ b)         = exprs b
            subExprs (ELam _ b)           = exprs b
            subExprs (ELet bindings expr) = concatMap (exprs . snd) bindings ++ exprs expr
            subExprs (ECase match _ alts) = exprs match ++ concatMap (exprs . altRHS) alts
            subExprs _                    = []
