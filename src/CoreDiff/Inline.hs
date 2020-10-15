{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module CoreDiff.Inline where

import Control.Monad.Trans.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import CoreDiff.XAst

-- | Inlining without regard for preserving correctness.
-- Uses simple substitution.
-- The bindings for the binders to inline need to still be present in bindings.
-- We assume that the bindings are in dependency order, which means that we do not need to inline recursively.
inline :: Set (XBinder UD) -> [XBinding UD] -> [XBinding UD]
inline bindersToInline bindings =
  runReader (inline' bindings) (bindersToInline, Map.empty)

inline' :: [XBinding UD] ->
  Reader (Set (XBinder UD), Map (XBinder UD) (XExpr UD)) [XBinding UD]
inline' [] = return []
inline' (XBinding binder expr : rest) = do
  (binders, subst) <- ask
  let expr' = runReader (applySubst expr) subst
  if binder `Set.member` binders then
    local (addSubst binder expr') $ inline' rest
  else do
    rest' <- inline' rest
    return $ XBinding binder expr' : rest'

addSubst binder expr (binders, subst) =
  (binders, Map.insert binder expr subst)

-- | Apply a substitution (represented as a map) to an expression.
-- Variables are simply replaced without regard for preserving correctness.
applySubst :: XExpr UD -> Reader (Map (XBinder UD) (XExpr UD)) (XExpr UD)
applySubst (XVar binder) = do
  subst <- ask
  case Map.lookup binder subst of
    Nothing   -> return $ XVar binder
    Just expr -> return $ expr
applySubst (XApp f x) = XApp <$> applySubst f <*> applySubst x
applySubst (XTyLam binder expr) = XTyLam binder <$> local (removeFromSubst [binder]) (applySubst expr)
applySubst (XLam binder expr) = XLam binder <$> local (removeFromSubst [binder]) (applySubst expr)
applySubst (XLet bindings expr) = XLet <$> mapM goBinding bindings <*> go expr
  where
    goBinding (XBinding binder expr) = XBinding binder <$> go expr
    go expr = local (removeFromSubst [ binder | XBinding binder _ <- bindings ]) (applySubst expr)
applySubst (XCase scrut binder alts) = XCase <$> applySubst scrut <*> return binder <*> mapM goAlt alts
  where
    goAlt (XAlt altCon binders rhs) = XAlt altCon binders <$> go rhs
      where go expr = local (removeFromSubst (binder : binders)) (applySubst expr)
applySubst x = return x

removeFromSubst binders subst = Map.withoutKeys subst (Set.fromList binders)
