{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module CoreDiff.Preprocess where

import Control.Monad.State.Lazy -- TODO: is Lazy important/bad here?
import Control.Monad.Reader
import Data.List
import Unsafe.Coerce
import qualified Data.Text as T
import GhcDump.Ast

import CoreDiff.Diff

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

-- Un-De-Bruijn terms after diffing:

class UnDeBruijn a where
  undoDeBruijn :: a -> Reader [Binder] a
  undoDeBruijn = udb
  udb :: a -> Reader [Binder] a

instance UnDeBruijn (Binder, Expr) where
  udb (bndr, expr) = (,) <$> udb bndr <*> udb expr

instance UnDeBruijn Binder where
  udb (Bndr bndr) = do
    binders <- ask
    let Bndr named = binders !! index
    return $ Bndr $ bndr { binderName = binderName named, binderId = binderId named }
    where BinderId (Unique 'b' index) = binderId bndr

instance UnDeBruijn Expr where
  udb (EVar v) = EVar <$> udb v
  udb (EVarGlobal extName) = return $ EVarGlobal extName
  udb (ELit lit) = return $ ELit lit
  udb (EApp f x) = EApp <$> udb f <*> udb x
  udb (ETyLam p b) = ETyLam <$> udb p <*> udb b
  udb (ELam p b) = ELam <$> udb p <*> udb b
  udb (ELet bindings expr) = ELet <$> mapM udb bindings <*> udb expr
  udb (ECase match bndr alts) = ECase <$> udb match <*> udb bndr <*> mapM udb alts
  udb (EType ty) = return $ EType ty
  udb (ECoercion) = return $ ECoercion

instance UnDeBruijn Alt where
  udb (Alt con binders rhs) = Alt con <$> mapM udb binders <*> udb rhs

-- change instance

instance UnDeBruijn t => UnDeBruijn (Change t) where
  udb (Change (lhs, rhs)) = do
    lhs' <- udb lhs
    rhs' <- udb rhs
    return $ Change (lhs', rhs')

-- diff instances

instance UnDeBruijn (Diff BindingC) where
  udb (BindingC bndr expr) = BindingC <$> udb bndr <*> udb expr
  udb (BindingHole bndgChg) = BindingHole <$> udb bndgChg

instance UnDeBruijn (Diff BndrC) where
  udb (BndrC bndr) = BndrC <$> udb bndr
  udb (BndrHole bndrChg) = BndrHole <$> udb bndrChg

instance UnDeBruijn (Diff ExprC) where
  udb (EVarC v) = EVarC <$> udb v
  udb (EVarGlobalC extName) = return $ EVarGlobalC extName
  udb (ELitC lit) = return $ ELitC lit
  udb (EAppC f x) = EAppC <$> udb f <*> udb x
  udb (ETyLamC p b) = ETyLamC <$> udb p <*> udb b
  udb (ELamC p b) = ELamC <$> udb p <*> udb b
  udb (ELetC bindings expr) = ELetC <$> mapM udb bindings <*> udb expr
  udb (ECaseC match bndr alts) = ECaseC <$> udb match <*> udb bndr <*> mapM udb alts
  udb (ETypeC ty) = return $ ETypeC ty
  udb (ECoercionC) = return $ ECoercionC
  udb (ExprHole exprChg) = ExprHole <$> udb exprChg

instance UnDeBruijn (Diff AltC) where
  udb (AltC con binders rhs) = AltC con <$> mapM udb binders <*> udb rhs
  udb (AltHole altChg) = AltHole <$> udb altChg
