{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module CoreDiff.Assimilate where

import Control.Monad
import Control.Monad.Trans.Reader

import CoreDiff.Pairing
import CoreDiff.PrettyPrint
import CoreDiff.XAst

import GhcDump.Ast

type Permutation = [(XBinderUniqueName, XBinderUniqueName)]

mkPerm binder binder' = (xBinderUniqueName binder, xBinderUniqueName binder')

permutePairingsInRhs :: PairingS -> PairingS
permutePairingsInRhs (PairingS pq unpairedL unpairedR paired) = PairingS
  (fmap (mapSnd go) pq)
  unpairedL
  (fmap go unpairedR)
  (map (mapSnd go) paired)
  where
    perm =
      [ mkPerm binder binder'
      | (XBinding binder _, XBinding binder' _) <- paired
      ]
    go t = runReader (applyPerm t) perm
    mapSnd f (a, b) = (a, f b)


permutePaired pairingS = pairingS { paired = map go $ paired pairingS }
  where
    go (XBinding binder expr, XBinding binder' expr') =
      ( XBinding binder expr
      , XBinding binder' (runReader (assimilate expr expr') [])
      )

class Asim a where
  assimilate :: a -> a -> Reader Permutation a

instance Asim (XExpr UD) where
  assimilate (XVar binder) (XVar binder') =
    XVar <$> applyPerm binder'
  assimilate (XApp f x) (XApp f' x') =
    XApp <$> assimilate f f' <*> assimilate x x'
  assimilate (XTyLam binder expr) (XTyLam binder' expr') =
    XTyLam <$> withPerm (applyPerm binder') <*> withPerm (assimilate expr expr')
    where withPerm = local (++ [mkPerm binder binder'])
  assimilate (XLam binder expr) (XLam binder' expr') =
    XLam <$> withPerm (applyPerm binder') <*> withPerm (assimilate expr expr')
    where withPerm = local (++ [mkPerm binder binder'])
  assimilate a@(XLet bindings expr) b@(XLet bindings' expr')
    -- TODO: this is just for debugging
    | length (paired pairings) /= length bindings' = error $ show 
      [ runReader (pprWithOpts a) pprOptsDefault
      , runReader (pprWithOpts b) pprOptsDefault
      , runReader (pprWithOpts pairings) pprOptsDefault
      ]
    | otherwise = XLet <$> zipWithM go bindings bindings' <*> withPerm (assimilate expr expr')
    where
      pairings = pairLet (expr, expr') (bindings, bindings')
      newPerm = [ mkPerm binder binder' | (XBinding binder _, XBinding binder' _) <- paired pairings ]
      withPerm = local (++ newPerm)
      go (XBinding binder expr) (XBinding binder' expr') =
        XBinding <$> withPerm (applyPerm binder') <*> withPerm (assimilate expr expr')
  assimilate (XCase scrut binder alts) (XCase scrut' binder' alts')
    -- TODO: depends on ordering of case alternatives
    | length alts /= length alts' = error "Mismatched number of alternatives"
    | otherwise =
      XCase <$> assimilate scrut scrut' <*> withPerm (applyPerm binder') <*> withPerm (zipWithM assimilate alts alts')
      where withPerm = local (++ [mkPerm binder binder'])
  assimilate _ x = applyPerm x
  -- TODO: types, cause they can contain tyvars

instance Asim (XAlt UD) where
  -- TODO: as of right, this depends on the ordering of alternatives and that the number of their binders doesnt change.
  assimilate (XAlt altCon binders rhs) (XAlt altCon' binders' rhs')
    | length binders /= length binders' = error "Mismatched number of binders"
    | otherwise =
      XAlt altCon binders <$> local (++ (zipWith mkPerm binders binders')) (assimilate rhs rhs')

class Perm a where
  applyPerm :: a -> Reader Permutation a

instance Perm (XBinder UD) where
  applyPerm binder = do
    perm <- ask
    return $ foldl go binder perm
    where
      go binder (a1, a2)
        | xBinderUniqueName binder == a1 = xBinderSetUniqueName a2 binder
        | xBinderUniqueName binder == a2 = xBinderSetUniqueName a1 binder
        | otherwise                  = binder

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
  -- Not yet implemented, possibly should be.
  applyPerm (XType ty) = return $ XType ty
  applyPerm x = return x
