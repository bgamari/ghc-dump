{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module CoreDiff.Assimilate where

import Control.Monad
import Control.Monad.Trans.Reader

import CoreDiff.Pairing
import CoreDiff.XAst

type Permutation = [(XBinder UD, XBinder UD)]


permutePairingsInRhs :: PairingS -> PairingS
permutePairingsInRhs (PairingS pq unpairedL unpairedR paired) = PairingS
  (fmap (mapSnd go) pq)
  unpairedL
  (fmap go unpairedR)
  (map (mapSnd go) paired)
  where
    perm = [ (binder, binder') | (XBinding binder _, XBinding binder' _) <- paired ]
    go t = runReader (applyPerm t) perm
    mapSnd f (a, b) = (a, f b)


permutePaired pairingS = pairingS { paired = map go $ paired pairingS }
  where
    go (binding@(XBinding binder expr), XBinding binder' expr') =
      ( binding
      , XBinding
        --(runReader (assimilate binder binder') [])
        binder
        (runReader (assimilate expr   expr')   [])
      )

class Asim a where
  assimilate :: a -> a -> Reader Permutation a

instance Asim (XExpr UD) where
  assimilate (XApp f x) (XApp f' x') =
    XApp <$> assimilate f f' <*> assimilate x x'
  assimilate (XTyLam binder expr) (XTyLam binder' expr') =
    XTyLam binder <$> local (++ [(binder, binder')]) (assimilate expr expr')
  assimilate (XLam binder expr) (XLam binder' expr') =
    XLam binder <$> local (++ [(binder, binder')]) (assimilate expr expr')
  assimilate (XLet bindings expr) (XLet bindings' expr') = error "let stub"
  assimilate (XCase scrut binder alts) (XCase scrut' binder' alts') =
    XCase <$> assimilate scrut scrut' <*> return binder <*> local (++ [(binder, binder')]) (zipWithM assimilate alts alts')
  assimilate _ x = applyPerm x

instance Asim (XAlt UD) where
  -- TODO: as of right, this depends on the ordering of alternatives and that the number of their binders doesnt change.
  assimilate (XAlt altCon binders rhs) (XAlt altCon' binders' rhs') =
    -- TODO: what if 
    XAlt altCon binders <$> local (++ (zip binders binders')) (assimilate rhs rhs')

class Perm a where
  applyPerm :: a -> Reader Permutation a

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

instance Perm (XBinder UD) where
  applyPerm binder = do
    perm <- ask
    return $ foldl go binder perm
    where
      go binder (a1, a2)
        | binder == a1 = a2
        | binder == a2 = a1
        | otherwise    = binder
