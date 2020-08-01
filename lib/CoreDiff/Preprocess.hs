{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module CoreDiff.Preprocess where

import Control.Monad.State.Lazy -- TODO: is Lazy important/bad here?
import Control.Monad.Reader
import Data.List
import qualified Data.Text as T
import GhcDump.Ast

import CoreDiff.XAst

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

-- assmimilate terms by finding binder name that can be paired up

data XBinderUniqueName = XBinderUniqueName T.Text BinderId deriving (Eq, Show)

getUName :: XBinder a -> XBinderUniqueName
getUName binder = XBinderUniqueName (xBinderName binder) (xBinderId binder)

setUName :: XBinder a -> XBinderUniqueName -> XBinder a
setUName binder (XBinderUniqueName name id) =
  binder { xBinderName = name, xBinderId = id }

swapNamesTopLvl :: XBinding UD -> XBinding UD -> XBinding UD
swapNamesTopLvl l@(XBinding lBinder _) r@(XBinding rBinder _) =
  runReader (swapNames l r) [(getUName lBinder, getUName rBinder)]

class Swap a where
  swapNames :: a -> a -> Reader [(XBinderUniqueName, XBinderUniqueName)] a
  applyPerm :: a -> Reader [(XBinderUniqueName, XBinderUniqueName)] a

instance Swap (XBinding UD) where
  swapNames (XBinding binder expr) (XBinding binder' expr') =
    XBinding <$> swapNames binder binder' <*> swapNames expr expr'

  applyPerm (XBinding binder expr) =
    XBinding <$> applyPerm binder <*> applyPerm expr

instance Swap (XBinder UD) where
  swapNames binder@XBinder{} binder'@XBinder{} = do
    ty' <- swapNames (xBinderType binder) (xBinderType binder')
    applyPerm $ binder' { xBinderType = ty' }
  swapNames binder@XTyBinder{} binder'@XTyBinder{} = do
    kind' <- swapNames (xBinderKind binder) (xBinderKind binder')
    applyPerm $ binder' { xBinderKind = kind' }
  swapNames _ r = applyPerm r

  applyPerm binder =
    foldl go binder <$> ask
    where
      go binder (lhs, rhs)
        | getUName binder == rhs = setUName binder lhs
        | otherwise              = binder

instance Swap (XExpr UD) where
  swapNames (XVar binder) (XVar binder') =
    XVar <$> swapNames binder binder'
  swapNames (XApp f x) (XApp f' x') =
    XApp <$> swapNames f f' <*> swapNames x x'
  swapNames (XTyLam binder expr) (XTyLam binder' expr') =
    XTyLam <$> withBndr (swapNames binder binder') <*> withBndr (swapNames expr expr')
    where withBndr = local (++ [(getUName binder, getUName binder')])
  swapNames (XLam binder expr) (XLam binder' expr') =
    XLam <$> withBndr (swapNames binder binder') <*> withBndr (swapNames expr expr')
    where withBndr = local (++ [(getUName binder, getUName binder')])
  -- If the both lets have the same number of bindings, assume that we can swap them
  -- otherwise, dont swap any of them (but still apply permutation)
  swapNames (XLet bindings expr) (XLet bindings' expr')
    | length bindings == length bindings' =
      XLet <$> withBndrs (zipWithM swapNames bindings bindings') <*> withBndrs (swapNames expr expr')
    | otherwise = error "TODO, lazy programmer"
    where withBndrs = local (++ zipWith go bindings bindings')
          go (XBinding bndr _) (XBinding bndr' _) = (getUName bndr, getUName bndr')
  -- same problem as above with the alts
  swapNames (XCase match binder alts) (XCase match' binder' alts')
    | length alts == length alts' =
      XCase <$> swapNames match match' <*> withBndr (swapNames binder binder') <*> zipWithM swapNames alts alts'
    | otherwise = error "TODO, lazy programmer"
    where withBndr = local (++ [(getUName binder, getUName binder')])
  swapNames (XType ty) (XType ty') =
    XType <$> swapNames ty ty'
  swapNames _ r = applyPerm r
  
  applyPerm (XVar binder) = XVar <$> applyPerm binder
  applyPerm (XApp f x) = XApp <$> applyPerm f <*> applyPerm x
  -- TODO: do we need to apply the permutation to binders here?
  applyPerm (XTyLam binder expr) = XTyLam <$> applyPerm binder <*> applyPerm expr
  applyPerm (XLam binder expr) = XLam <$> applyPerm binder <*> applyPerm expr
  applyPerm (XLet bindings expr) = XLet <$> mapM applyPerm bindings <*> applyPerm expr
  applyPerm (XCase match binder expr) = XCase <$> applyPerm match <*> applyPerm binder <*> mapM applyPerm expr
  applyPerm (XType ty) = XType <$> applyPerm ty
  applyPerm expr = return expr

instance Swap (XAlt UD) where
  -- aaand same problem again
  swapNames (XAlt altCon binders rhs) (XAlt altCon' binders' rhs')
    | length binders == length binders' =
      XAlt altCon' <$> withBinders (zipWithM swapNames binders binders') <*> withBinders (swapNames rhs rhs')
    | otherwise = error "TODO, lazy programmer"
    where withBinders = local (++ zipWith go binders binders')
          go binder binder' = (getUName binder, getUName binder')

  applyPerm (XAlt altCon binders rhs) = XAlt altCon <$> mapM applyPerm binders <*> applyPerm rhs


instance Swap (XType UD) where
  swapNames (XVarTy binder)       (XVarTy binder')        =
    XVarTy <$> swapNames binder binder'
  swapNames (XFunTy l r)          (XFunTy l' r')          =
    XFunTy <$> swapNames l l' <*> swapNames r r'
  swapNames (XTyConApp tc args)   (XTyConApp tc' args')   =
    XTyConApp tc' <$> zipWithM swapNames args args'
  swapNames (XAppTy f x)          (XAppTy f' x')          =
    XAppTy <$> swapNames f f' <*> swapNames x x'
  swapNames (XForAllTy binder ty) (XForAllTy binder' ty') =
    XForAllTy <$> withBndr (swapNames binder binder') <*> withBndr (swapNames ty ty')
    where withBndr = local (++ [(getUName binder, getUName binder')])
  swapNames _ r = applyPerm r

  applyPerm (XVarTy binder) = XVarTy <$> applyPerm binder
  applyPerm (XFunTy l r) = XFunTy <$> applyPerm l <*> applyPerm r
  applyPerm (XTyConApp tc args) = XTyConApp tc <$> mapM applyPerm args
  applyPerm (XAppTy f x) = XAppTy <$> applyPerm f <*> applyPerm x
  applyPerm (XForAllTy binder ty) = XForAllTy <$> applyPerm binder <*> applyPerm ty
  applyPerm ty = return ty
