{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module CoreDiff.Preprocess where

import Control.Monad.State
import Control.Monad.Reader
import Data.List
import qualified Data.Text as T
import GhcDump.Ast

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CoreDiff.XAst


swapNamesTopLvl :: XBinding UD -> XBinding UD -> XBinding UD
swapNamesTopLvl l@(XBinding lBinder _) r@(XBinding rBinder _) =
  runReader (swapNames l r) [(xBinderUName lBinder, xBinderUName rBinder)]

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
        | xBinderUName binder == rhs = xBinderSetUName binder lhs
        | otherwise              = binder

instance Swap (XExpr UD) where
  swapNames (XVar binder) (XVar binder') =
    XVar <$> swapNames binder binder'
  swapNames (XApp f x) (XApp f' x') =
    XApp <$> swapNames f f' <*> swapNames x x'
  swapNames (XTyLam binder expr) (XTyLam binder' expr') =
    XTyLam <$> withBndr (swapNames binder binder') <*> withBndr (swapNames expr expr')
    where withBndr = local (++ [(xBinderUName binder, xBinderUName binder')])
  swapNames (XLam binder expr) (XLam binder' expr') =
    XLam <$> withBndr (swapNames binder binder') <*> withBndr (swapNames expr expr')
    where withBndr = local (++ [(xBinderUName binder, xBinderUName binder')])
  -- If the both lets have the same number of bindings, assume that we can swap them
  -- otherwise, dont swap any of them (but still apply permutation)
  swapNames (XLet bindings expr) (XLet bindings' expr')
    | length bindings == length bindings' =
      XLet <$> withBndrs (zipWithM swapNames bindings bindings') <*> withBndrs (swapNames expr expr')
    | otherwise =
      XLet <$> mapM applyPerm bindings' <*> applyPerm expr'
    where withBndrs = local (++ zipWith go bindings bindings')
          go (XBinding bndr _) (XBinding bndr' _) = (xBinderUName bndr, xBinderUName bndr')
  -- Same thing as above: If two cases dont have the same number of alts,
  -- we won't even try to pair them up nicely; we will just apply the permutation
  swapNames (XCase match binder alts) (XCase match' binder' alts')
    | length alts == length alts' =
      XCase <$> swapNames match match' <*> withBndr (swapNames binder binder') <*> withBndr (zipWithM swapNames alts alts')
    | otherwise =
      XCase <$> applyPerm match' <*> applyPerm binder' <*> mapM applyPerm alts'
    where withBndr = local (++ [(xBinderUName binder, xBinderUName binder')])
  swapNames (XType ty) (XType ty') =
    XType <$> swapNames ty ty'
  swapNames _ r = applyPerm r
  
  applyPerm (XVar binder) = XVar <$> applyPerm binder
  applyPerm (XApp f x) = XApp <$> applyPerm f <*> applyPerm x
  applyPerm (XTyLam binder expr) = XTyLam <$> applyPerm binder <*> applyPerm expr
  applyPerm (XLam binder expr) = XLam <$> applyPerm binder <*> applyPerm expr
  applyPerm (XLet bindings expr) = XLet <$> mapM applyPerm bindings <*> applyPerm expr
  applyPerm (XCase match binder expr) = XCase <$> applyPerm match <*> applyPerm binder <*> mapM applyPerm expr
  applyPerm (XType ty) = XType <$> applyPerm ty
  applyPerm expr = return expr

instance Swap (XAlt UD) where
  -- aaand same problem for different number of binders
  swapNames (XAlt altCon binders rhs) (XAlt altCon' binders' rhs')
    | length binders == length binders' =
      XAlt altCon' <$> withBinders (zipWithM swapNames binders binders') <*> withBinders (swapNames rhs rhs')
    | otherwise =
      XAlt altCon' <$> mapM applyPerm binders' <*> applyPerm rhs
    where withBinders = local (++ zipWith go binders binders')
          go binder binder' = (xBinderUName binder, xBinderUName binder')

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
    where withBndr = local (++ [(xBinderUName binder, xBinderUName binder')])
  swapNames _ r = applyPerm r

  applyPerm (XVarTy binder) = XVarTy <$> applyPerm binder
  applyPerm (XFunTy l r) = XFunTy <$> applyPerm l <*> applyPerm r
  applyPerm (XTyConApp tc args) = XTyConApp tc <$> mapM applyPerm args
  applyPerm (XAppTy f x) = XAppTy <$> applyPerm f <*> applyPerm x
  applyPerm (XForAllTy binder ty) = XForAllTy <$> applyPerm binder <*> applyPerm ty
  applyPerm ty = return ty
 

data FloatInState = FloatInState
  { bindersToFloatIn :: Map (XBinder UD) (XExpr UD)
  , usedBinders :: Set (XBinder UD)
  }

initFloatInState = FloatInState Map.empty Set.empty

addBindersToFloatIn :: [XBinding UD] -> State FloatInState ()
addBindersToFloatIn bindings = modify go
  where
    go s = s { bindersToFloatIn = Map.union (bindersToFloatIn s) bindingsMap }
    bindingsMap =
      Map.fromList [(binder, expr) | XBinding binder expr <- bindings ]

markAsUsed :: XBinder UD -> State FloatInState ()
markAsUsed binder = modify go
  where
    go s = s { usedBinders = Set.insert binder $ usedBinders s }

getUsedBinders :: State FloatInState (Set (XBinder UD))
getUsedBinders = usedBinders <$> get


floatInTopLvl :: [XBinding UD] -> [XBinding UD]
floatInTopLvl bindings =
  evalState (floatInTopLvl' p bindings) initFloatInState
  where
    p = filter go
      where go (XBinding binder _ ) = xBinderName binder `elem` ["lvl", "$wlvl"]

-- Given a function that selects bindings to float in, this function
-- replaces each occurrence of the given binders with their bound expr.
-- Bindings that have actually been substituted are removed from the
-- resulting bindings list
floatInTopLvl'
  :: ([XBinding UD] -> [XBinding UD])
  -> [XBinding UD]
  -> State FloatInState [XBinding UD]
floatInTopLvl' selector bindings = do
  addBindersToFloatIn $ selector bindings
  bindings' <- mapM (floatIn selector) bindings
  usedBinders <- getUsedBinders
  return [b | b@(XBinding binder _) <- bindings', not $ binder `elem` usedBinders]

class FloatIn a where
  floatIn :: ([XBinding UD] -> [XBinding UD]) -> a -> State FloatInState a

instance FloatIn (XBinding UD) where
  floatIn selector (XBinding binder expr) =
    XBinding <$> floatIn selector binder <*> floatIn selector expr

instance FloatIn (XBinder UD) where
  floatIn _ binder = return binder

instance FloatIn (XExpr UD) where
  floatIn selector (XVar binder) = do
    binders <- bindersToFloatIn <$> get
    case Map.lookup binder binders of
      Nothing -> return $ XVar binder
      Just expr -> do
        markAsUsed binder
        floatIn selector expr

  floatIn selector (XApp f x) =
    XApp <$> floatIn selector f <*> floatIn selector x

  -- lambdas should be irrelevant to float-in, don't add binder
  floatIn selector (XTyLam p b) =
    XTyLam <$> floatIn selector p <*> floatIn selector b
  floatIn selector (XLam p b) =
    XLam <$> floatIn selector p <*> floatIn selector b

  -- this is the most important part of the whole float-in stuff
  floatIn selector (XLet bindings expr) = do
    -- new lvl-binders etc. from this let are marked as "to float in"
    addBindersToFloatIn $ selector bindings
    -- bindings and expression are recursed into
    bindings' <- mapM (floatIn selector) bindings
    expr' <- floatIn selector expr
    -- binders that have been used in the bindings and expression above are marked as used
    usedBinders <- getUsedBinders
    -- only bindings that havent been used are retained
    let bindings'' = [b | b@(XBinding binder _) <- bindings', not $ binder `elem` usedBinders]

    -- if no bindings are left, return the expression itself
    if bindings'' == [] then
      return expr'
    else
      return $ XLet bindings'' expr'

  -- cases should be irrelevant to float-in, don't add binder
  floatIn pred (XCase match binder alts) = do
    XCase <$> floatIn pred match <*> floatIn pred binder <*> mapM (floatIn pred) alts

  floatIn pred (XType ty) = XType <$> floatIn pred ty

  floatIn _ other = return other

instance FloatIn (XType UD) where
  -- TODO: can stuff even *be* floated out of types?
  -- I guess $krep stuff and such, why shouldn't be relevant tho
  -- A: Don't think so, at least it wouldn't be relevant.
  floatIn _ ty = return ty

instance FloatIn (XAlt UD) where
  floatIn pred (XAlt con binders rhs) =
    XAlt con binders <$> floatIn pred rhs
