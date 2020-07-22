{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module CoreDiff.Preprocess where

import Control.Monad.State.Lazy -- TODO: is Lazy important/bad here?
import Data.List
import Unsafe.Coerce
import qualified Data.Text as T

import GhcDump.Ast

-- Converting SExpr to Expr, SBndr to Binder, etc.

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

-- Calculate De-Bruijn index for terms

class DeBruijn a where
  dbi :: a -> State [Binder] a
  
  deBruijnIndex :: a -> State [Binder] a
  deBruijnIndex = dbi

instance DeBruijn Binder where
  dbi wrapper@(Bndr bndr) = do
    bndrs <- get
    let dbId = BinderId $ Unique 'b' $ fromJust $ findIndex (== wrapper) bndrs
    return $ Bndr $ bndr { binderName = "", binderId = dbId }
    where fromJust (Just x) = x
          fromJust Nothing = error $ T.unpack $ binderUniqueName wrapper

-- Doesn't take care of making sure that bndr appears in the binders for expr
-- Code that handles top-level bindings/bindings in let takes care of that
instance DeBruijn (Binder, Expr) where
  dbi (bndr, expr) = (,) <$> dbi bndr <*> dbi expr
  -- This is all kinds of cool and point-free but i feel like do would be a little clearer

instance DeBruijn Expr where
  dbi (EVar v) = EVar <$> dbi v
  dbi (EVarGlobal extName) = return $ EVarGlobal extName
  dbi (ELit lit) = return $ ELit lit
  dbi (EApp f x) = EApp <$> dbi f <*> dbi x
  dbi (ETyLam param body) = do
    modify (++ [param])
    param' <- dbi param
    body' <- dbi body
    return $ ETyLam param' body'
  dbi (ELam param body) = do
    modify (++ [param])
    param' <- dbi param
    body' <- dbi body
    return $ ELam param' body'
  dbi (ELet bindings expr) = do
    modify (++ (map fst bindings))
    bindings' <- mapM dbi bindings
    expr' <- dbi expr
    return $ ELet bindings' expr'
  dbi (ECase match bndr alts) = do
    match' <- dbi match
    modify (++ [bndr])
    bndr' <- dbi bndr
    alts' <- mapM dbi alts
    return $ ECase match' bndr' alts'
  dbi (EType ty) = return $ EType ty
  dbi (ECoercion) = return ECoercion

instance DeBruijn Alt where
  dbi (Alt con binders rhs) = do
    modify (++ binders)
    binders' <- mapM dbi binders
    rhs' <- dbi rhs
    return $ Alt con binders' rhs'
