module CoreDiff.Diff where

import GhcDump.Ast

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
  | EType (TypeC metavar)
  | ECoercionC
  | CoreHole metavar

data AltC metavar = AltC
  { altCon :: AltCon
  , altBinders :: [SBinder]
  , altRHS :: CoreC metavar
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

diffCore :: SExpr -> SExpr -> CorePatch
diffCore = error "unimplemented"
