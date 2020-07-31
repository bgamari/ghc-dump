{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module CoreDiff.Preprocess where

import Control.Monad.State.Lazy -- TODO: is Lazy important/bad here?
import Control.Monad.Reader
import Data.List
import Unsafe.Coerce
import qualified Data.Text as T
import GhcDump.Ast

import CoreDiff.XAst

-- Converting SExpr to Expr, SBndr to Binder, etc.

sbindingToBinding env (bndr, expr) =
  (sbinderToBinder env bndr, sexprToExpr env expr)

-- TODO: convert to Reader
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
        bindings' = map (sbindingToBinding env') bindings
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
  where kind' = stypeToType env $ binderKind bndr

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
        fromJust Nothing = error $ "missing: " ++ show id

-- Calculate De-Bruijn index for terms

class DeBruijn a where
  dbi :: a -> State [XBinder UD] a
  
  deBruijnIndex :: a -> State [XBinder UD] a
  deBruijnIndex = dbi


-- Doesn't take care of making sure that bndr appears in the binders for expr
-- Code that handles top-level bindings/bindings in let takes care of that
instance DeBruijn (XBinding UD) where
  dbi (XBinding binder expr) = XBinding <$> dbi binder <*> dbi expr
  -- This is all kinds of cool and point-free but i feel like do would be a little clearer

instance DeBruijn (XBinder UD) where
  dbi binder@XBinder{} = do
    binders <- get
    let dbId = BinderId $ Unique 'b' $ fromJust $ findIndex (== binder) binders
    dbType <- dbi $ xBinderType binder
    return $ binder { xBinderName = "", xBinderId = dbId, xBinderType = dbType }
    where fromJust (Just x) = x
          -- fromJust Nothing = error $ T.unpack $ xBinderUniqueName binder
          fromJust Nothing = -1
  dbi binder@XTyBinder{} = do
    binders <- get
    let dbId = BinderId $ Unique 'b' $ fromJust $ findIndex (== binder) binders
    dbKind <- dbi $ xBinderKind binder
    return $ binder { xBinderName = "", xBinderId = dbId, xBinderKind = dbKind }
    where fromJust (Just x) = x
          fromJust Nothing = error $ T.unpack $ xBinderUniqueName binder
-- TODO: reduce code duplication

instance DeBruijn (XExpr UD) where
  dbi (XVar binder) = XVar <$> dbi binder
  dbi (XVarGlobal extName) = return $ XVarGlobal extName
  dbi (XLit lit) = return $ XLit lit
  dbi (XApp f x) = XApp <$> dbi f <*> dbi x
  dbi (XTyLam param body) = do
    modify (++ [param])
    param' <- dbi param
    body' <- dbi body
    return $ XTyLam param' body'
  dbi (XLam param body) = do
    modify (++ [param])
    param' <- dbi param
    body' <- dbi body
    return $ XLam param' body'
  dbi (XLet bindings expr) = do
    modify (++ map go bindings)
    bindings' <- mapM dbi bindings
    expr' <- dbi expr
    return $ XLet bindings' expr'
    where go (XBinding binder _) = binder
  dbi (XCase match bndr alts) = do
    match' <- dbi match
    modify (++ [bndr])
    bndr' <- dbi bndr
    alts' <- mapM dbi alts
    return $ XCase match' bndr' alts'
  dbi (XType ty) = XType <$> dbi ty
  dbi (XCoercion) = return XCoercion

instance DeBruijn (XAlt UD) where
  dbi (XAlt con binders rhs) = do
    modify (++ binders)
    binders' <- mapM dbi binders
    rhs' <- dbi rhs
    return $ XAlt con binders' rhs'

instance DeBruijn (XType UD) where
  dbi (XVarTy binder) = XVarTy <$> dbi binder
  dbi (XFunTy l r) = XFunTy <$> dbi l <*> dbi r
  dbi (XTyConApp tc args) = XTyConApp tc <$> mapM dbi args
  dbi (XAppTy f x) = XAppTy <$> dbi f <*> dbi x
  dbi (XForAllTy binder ty) = do
    modify (++ [binder])
    XForAllTy <$> dbi binder <*> dbi ty
  dbi (XLitTy) = return $ XLitTy
  dbi (XCoercionTy) = return $ XCoercionTy

{-
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
-}
