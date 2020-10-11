{-# LANGUAGE DataKinds #-}

module CoreDiff.Convert where

import GhcDump.Ast

import CoreDiff.XAst

cvtXModule :: Module -> XModule
cvtXModule mod = XModule
  { xModuleName = getModuleName $ moduleName mod
  , xModulePhase = modulePhase mod
  , xModuleBindings = map cvtXBinding $ map removeStats $ moduleBindings mod
  }
  where removeStats (binder, _stats, expr) = (binder, expr)

cvtXBinding :: (Binder, Expr) -> XBinding UD
cvtXBinding bind = error "stub"
