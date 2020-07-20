module CoreDiff.Preprocess where

import Data.List
import Unsafe.Coerce

import GhcDump.Ast

sexprToExpr :: [Binder] -> SExpr -> Expr
sexprToExpr = go
  where
    go :: [Binder] -> SExpr -> Expr
    go env (EVar bndrId) = EVar (lookupBndrId bndrId env)
    go env (EVarGlobal extName) = EVarGlobal extName
    go env (ELit lit) = ELit lit
    go env (EApp f x) = EApp (go env f) (go env x)
    go env (ETyLam bndr expr) = ETyLam bndr' (go (bndr' : env) expr)
      where bndr' = sbinderToBinder env bndr
    go env (ELam bndr expr) = ELam bndr' (go (bndr' : env) expr)
      where bndr' = sbinderToBinder env bndr
    go env (ELet bindings expr) = ELet bindings' (go env' expr)
      where
        env' = binders ++ env
        binders = map (sbinderToBinder env' . fst) bindings
        bindings' = map cvtBinding bindings
        cvtBinding (bndr, expr) =
          (sbinderToBinder env' bndr, go env' expr)
    go env (ECase match bndr alts) = ECase
      (go env match)
      (sbinderToBinder env bndr)
      (map (saltToAlt env') alts)
      where
        env' = sbinderToBinder env bndr : env
    go env (EType ty) = EType (stypeToType env ty)
    go env (ECoercion) = ECoercion

sbinderToBinder :: [Binder] -> SBinder -> Binder
sbinderToBinder env (SBndr (bndr@Binder {})) = Bndr $ bndr
  { binderIdInfo = idInfo'
  , binderType = ty'
  }
  where idInfo' = cvtIdInfo $ binderIdInfo bndr
        ty' = stypeToType env $ binderType bndr
        cvtIdInfo (idi@IdInfo {}) =
          idi { idiUnfolding = cvtUnf $ idiUnfolding idi }
        cvtUnf (cu@CoreUnfolding {}) =
          cu { unfTemplate = sexprToExpr env $ unfTemplate cu }
        cvtUnf otherUnf = unsafeCoerce otherUnf -- TODO: mehhhh
sbinderToBinder env (SBndr (bndr@TyBinder {})) = Bndr $ bndr
  { binderKind = kind'
  }
  where kind' = stypeToType env $ binderType bndr

saltToAlt env alt = alt { altBinders = binders', altRHS = rhs' }
  where
    binders' = map (sbinderToBinder env) (altBinders alt)
    env' = binders' ++ env
    rhs' = sexprToExpr env' (altRHS alt)

stypeToType :: [Binder] -> SType -> Type
stypeToType = go
  where
    go env (VarTy var) = VarTy $ lookupBndrId var env
    go env (FunTy a b) = FunTy (go env a) (go env b)
    go env (TyConApp tc args) = TyConApp tc $ map (go env) args
    go env (AppTy f x) = AppTy (go env f) (go env x)
    go env (ForAllTy bndr ty) =
      ForAllTy bndr' (go (bndr' : env) ty)
      where bndr' = sbinderToBinder env bndr
    go env (LitTy) = LitTy
    go env (CoercionTy) = CoercionTy

lookupBndrId :: BinderId -> [Binder] -> Binder
lookupBndrId id = fromJust . find go
  where go bndr = id == binderId (unBndr bndr)
        fromJust (Just x) = x
