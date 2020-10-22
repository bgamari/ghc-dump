{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module CoreDiff.Assimilate where

import GhcDump.Ast
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Trans.Reader

import CoreDiff.Pairing
import CoreDiff.PrettyPrint
import CoreDiff.XAst

type Permutation = [(XBinderUniqueName, XBinderUniqueName)]

mkPerm binder binder' = (xBinderUniqueName binder, xBinderUniqueName binder')

permutePairingsInRhs :: PairingS -> PairingS
permutePairingsInRhs (PairingS pq unpairedL unpairedR paired) = PairingS
  (fmap (mapSnd go) pq)
  unpairedL
  (Map.mapKeys go (fmap go unpairedR))
  (map (mapSnd go) paired)
  where
    perm =
      [ mkPerm binder binder'
      | (XBinding binder _, XBinding binder' _) <- paired
      ]
    go t = runReader (applyPerm t) perm
    mapSnd f (a, b) = (a, f b)


assimilatePaired pairingS = pairingS { paired = map go $ paired pairingS }
  where
    go (XBinding binder expr, XBinding binder' expr') =
      ( XBinding binder expr
      , XBinding (runReader (assimilate binder binder') []) (runReader (assimilate expr expr') [])
      )

class Asim a where
  assimilate :: a -> a -> Reader Permutation a

instance Asim (XExpr UD) where
  assimilate (XVar binder) (XVar binder') =
    XVar <$> assimilate binder binder'
  assimilate (XApp f x) (XApp f' x') =
    XApp <$> assimilate f f' <*> assimilate x x'
  assimilate (XTyLam binder expr) (XTyLam binder' expr') =
    XTyLam <$> withPerm (assimilate binder binder') <*> withPerm (assimilate expr expr')
    where withPerm = local (++ [mkPerm binder binder'])
  assimilate (XLam binder expr) (XLam binder' expr') =
    XLam <$> withPerm (assimilate binder binder') <*> withPerm (assimilate expr expr')
    where withPerm = local (++ [mkPerm binder binder'])
  assimilate (XLet [XBinding binder bound] expr) (XLet [XBinding binder' bound'] expr') =
    XLet <$> binding <*> withPerm (assimilate expr expr')
    where
      withPerm = local (++ [mkPerm binder binder'])
      binding =
        fmap (: []) $ XBinding <$> withPerm (assimilate binder binder') <*> withPerm (assimilate bound bound')

  assimilate a@(XLet bindings expr) b@(XLet bindings' expr')
    -- TODO: Figure out what to do if the number of bindings doesn't match.
    | length bindings /= length bindings' = error "Mismatched number of bindings"
    | otherwise =
      XLet <$> zipWithM go bindings bindings' <*> withPerm (assimilate expr expr')
    where
      pairings = pairLet (expr, expr') (bindings, bindings')
      newPerm = [ mkPerm binder binder' | (XBinding binder _, XBinding binder' _) <- paired pairings ]
      withPerm = local (++ newPerm)
      go (XBinding binder expr) (XBinding binder' expr') =
        XBinding <$> withPerm (assimilate binder binder') <*> withPerm (assimilate expr expr')
  assimilate (XCase scrut binder alts) (XCase scrut' binder' alts')
    -- TODO: Figure out what to do if the number of alternatives doesn't match.
    | length alts /= length alts' = error "Mismatched number of alternatives"
    | otherwise =
      XCase <$> assimilate scrut scrut' <*> withPerm (assimilate binder binder') <*> withPerm (zipWithM assimilate alts alts')
    where withPerm = local (++ [mkPerm binder binder'])
  assimilate (XType ty) (XType ty') =
    XType <$> assimilate ty ty'
  assimilate _ x = applyPerm x

instance Asim (XAlt UD) where
  -- TODO: Figure out what to do if the number of binders doesn't match
  assimilate (XAlt altCon binders rhs) (XAlt altCon' binders' rhs')
    | length binders /= length binders' = error "Mismatched number of binders"
    | otherwise =
      XAlt altCon <$> withPerms (zipWithM assimilate binders binders') <*> withPerms (assimilate rhs rhs')
    where withPerms = local (++ zipWith mkPerm binders binders')

instance Asim (XBinder UD) where
  assimilate binder binder' = do
    tyOrKind'' <- assimilate tyOrKind tyOrKind'
    xBinderSetTypeOrKind tyOrKind'' <$> applyPerm binder'
    where
      tyOrKind  = xBinderTypeOrKind binder
      tyOrKind' = xBinderTypeOrKind binder'

instance Asim (XType UD) where
  assimilate (XVarTy binder) (XVarTy binder') =
    XVarTy <$> assimilate binder binder'
  assimilate (XFunTy l r) (XFunTy l' r') =
    XFunTy <$> assimilate l l' <*> assimilate r r'
  assimilate (XTyConApp tc tys) (XTyConApp tc' tys')
    | length tys /= length tys' = error "Mismatched number of types"
    | otherwise =
      XTyConApp tc' <$> zipWithM assimilate tys tys'
  assimilate (XAppTy l r) (XAppTy l' r') =
    XAppTy <$> assimilate l l' <*> assimilate r r'
  assimilate (XForAllTy binder ty) (XForAllTy binder' ty') =
    XForAllTy <$> withPerm (assimilate binder binder') <*> withPerm (assimilate ty ty')
    where withPerm = local (++ [mkPerm binder binder'])
  assimilate _ ty = applyPerm ty

class Perm a where
  applyPerm :: a -> Reader Permutation a

instance Perm (XBinder UD) where
  applyPerm binder = do
    ty' <- applyPerm $ xBinderTypeOrKind binder
    perms <- ask
    return $ xBinderSetTypeOrKind ty' $ foldl go binder perms
    where
      go binder (a1, a2)
        | xBinderUniqueName binder == a1 = xBinderSetUniqueName a2 binder
        | xBinderUniqueName binder == a2 = xBinderSetUniqueName a1 binder
        | otherwise                      = binder

instance Perm (XBinding UD) where
  applyPerm (XBinding binder expr) = XBinding <$> applyPerm binder <*> applyPerm expr

instance Perm (XExpr UD) where
  applyPerm (XVar binder) = XVar <$> applyPerm binder
  applyPerm (XApp f x) = XApp <$> applyPerm f <*> applyPerm x
  applyPerm (XTyLam binder expr) = XTyLam <$> applyPerm binder <*> applyPerm expr
  applyPerm (XLam   binder expr) = XLam   <$> applyPerm binder <*> applyPerm expr
  applyPerm (XLet bindings expr) = XLet <$> mapM applyPerm bindings <*> applyPerm expr
  applyPerm (XCase scrut binder alts) = XCase <$> applyPerm scrut <*> applyPerm binder <*> mapM go alts
    where go (XAlt altCon binders rhs) =
            XAlt altCon <$> mapM applyPerm binders <*> applyPerm rhs
  applyPerm (XType ty) = XType <$> applyPerm ty
  -- Other cases without sub-terms or binders
  applyPerm expr = return expr

instance Perm (XType UD) where
  applyPerm (XVarTy binder) = XVarTy <$> applyPerm binder
  applyPerm (XFunTy l r) = XFunTy <$> applyPerm l <*> applyPerm r
  applyPerm (XTyConApp tc tys) = XTyConApp tc <$> mapM applyPerm tys
  applyPerm (XAppTy l r) = XAppTy <$> applyPerm l <*> applyPerm r
  applyPerm (XForAllTy binder ty) = XForAllTy <$> applyPerm binder <*> applyPerm ty
  -- Other cases without sub-terms or binders
  applyPerm ty = return ty
