{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module CoreDiff.Diff where

import CoreDiff.XAst
import Data.List
import GhcDump.Ast (ExternalName(..), TyCon(..))
-- TODO: Sometimes we want to convert an XExpr UD to an XExpr Diff
-- Since XExpr UD never has any extensions, this is a no-op
-- The type system can't unify their types tho.
import Unsafe.Coerce

-- Do some steppos

diff :: XBinding UD -> XBinding UD -> XBinding Diff
diff lhs rhs = closureBinding $ gcpBinding lhs rhs

-- Calculate spine of two contexts a.k.a. their Greatest Common Prefix

gcpBinding :: XBinding UD -> XBinding UD -> XBinding Diff
gcpBinding (XBinding binder expr) (XBinding binder' expr') =
  XBinding (gcpBinder binder binder') (gcpExpr expr expr')


-- TODO: unfoldings?
-- TODO: Remove coercions
gcpBinder :: XBinder UD -> XBinder UD -> XBinder Diff
gcpBinder binder@XBinder{} binder'@XBinder{}
  | binder == binder' = unsafeCoerce binder
gcpBinder binder@XTyBinder{} binder'@XTyBinder{}
  | binder == binder' = unsafeCoerce binder
gcpBinder binder binder' =
  XXBinder $ Change (binder, binder')


gcpExpr :: XExpr UD -> XExpr UD -> XExpr Diff
gcpExpr (XVar binder) (XVar binder') =
  XVar $ gcpBinder binder binder'
gcpExpr (XVarGlobal extName) (XVarGlobal extName') | extName ~~ extName' =
  XVarGlobal extName
  where
    -- See note in CoreDiff.XAst's Eq (XExpr a) instance
    (ExternalName mod name _) ~~ (ExternalName mod' name' _) =
      mod == mod' && name == name'
gcpExpr (XLit lit) (XLit lit') | lit == lit' =
  XLit lit
gcpExpr (XApp f x) (XApp f' x') =
  XApp (gcpExpr f f') (gcpExpr x x')
gcpExpr (XTyLam p b) (XTyLam p' b') =
  XTyLam (gcpBinder p p') (gcpExpr b b')
gcpExpr (XLam p b) (XLam p' b') =
  XLam (gcpBinder p p') (gcpExpr b b')
gcpExpr (XLet bindings expr) (XLet bindings' expr') | length bindings == length bindings' =
  XLet (zipWith gcpBinding bindings bindings') (gcpExpr expr expr')
gcpExpr (XCase match binder alts) (XCase match' binder' alts') | length alts == length alts' =
  XCase (gcpExpr match match') (gcpBinder binder binder') (zipWith gcpAlt alts alts')
gcpExpr (XCoercion) (XCoercion) =
  XCoercion
gcpExpr (XType ty) (XType ty') =
  XType $ gcpType ty ty'
gcpExpr expr expr' =
  XXExpr $ Change (expr, expr')


gcpAlt :: XAlt UD -> XAlt UD -> XAlt Diff
gcpAlt (XAlt con binders rhs) (XAlt con' binders' rhs') | con == con' =
  XAlt con (zipWith gcpBinder binders binders') (gcpExpr rhs rhs')
gcpAlt alt alt' =
  XXAlt $ Change (alt, alt')


gcpType :: XType UD -> XType UD -> XType Diff
gcpType (XVarTy binder) (XVarTy binder') =
  XVarTy $ gcpBinder binder binder'
gcpType (XFunTy l r) (XFunTy l' r') =
  XFunTy (gcpType l l') (gcpType r r')
gcpType (XTyConApp tc args) (XTyConApp tc' args') | tc ~~ tc' =
  -- Possible solution: Check (TyCon tc []) == (TyCon tc' []), ugly but works.
  XTyConApp tc $ zipWith gcpType args args'
  where
    -- See note in CoreDiff.XAst's Eq (XExpr a) instance
    (TyCon name _) ~~ (TyCon name' _) =
      name == name'
gcpType (XAppTy f x) (XAppTy f' x') =
  XAppTy (gcpType f f') (gcpType x x')
gcpType (XForAllTy binder ty) (XForAllTy binder' ty') =
  XForAllTy (gcpBinder binder binder') (gcpType ty ty')
gcpType (XLitTy) (XLitTy) =
  XLitTy
gcpType (XCoercionTy) (XCoercionTy) =
  XCoercionTy
gcpType ty ty' =
  XXType $ Change (ty, ty')


-- Zip up terms that are different in all their subterms
-- E.g. (f/g) (x/y) ~> (f x/g h)

closureBinding :: XBinding Diff -> XBinding Diff
closureBinding (XBinding binder expr) = go binder' expr'
  where
    binder' = closureBinder binder
    expr' = closureExpr expr
    go (XXBinder (Change (b1, b2))) (XXExpr (Change (e1, e2))) =
      XXBinding $ Change (XBinding b1 e1, XBinding b2 e2)
    go _ _ =
      XBinding binder' expr'
closureBinding (XXBinding extension) = XXBinding extension

closureBinder :: XBinder Diff -> XBinder Diff
closureBinder = id

-- TODO: is this necessary for Var?
closureExpr (XVar binder) = go binder'
  where
    binder' = closureBinder binder
    go (XXBinder (Change (b1, b2))) =
      XXExpr $ Change (XVar b1, XVar b2)
    go _ =
      XVar binder'
closureExpr (XVarGlobal extName) = XVarGlobal extName
closureExpr (XLit lit) = XLit lit
closureExpr (XApp f x) = go f' x'
  where
    f' = closureExpr f
    x' = closureExpr x
    go (XXExpr (Change (f1, f2))) (XXExpr (Change (x1, x2))) =
      XXExpr $ Change (XApp f1 x1, XApp f2 x2)
    go _ _ =
      XApp f' x'
closureExpr (XTyLam p b) = go p' b'
  where
    p' = closureBinder p
    b' = closureExpr b
    go (XXBinder (Change (p1, p2))) (XXExpr (Change (b1, b2))) =
      XXExpr $ Change (XTyLam p1 b1, XTyLam p2 b2)
    go _ _ =
      XTyLam p' b'
closureExpr (XLam p b) = go p' b'
  where
    p' = closureBinder p
    b' = closureExpr b
    go (XXBinder (Change (p1, p2))) (XXExpr (Change (b1, b2))) =
      XXExpr $ Change (XLam p1 b1, XLam p2 b2)
    go _ _ =
      XLam p' b'
-- TODO: close up these Let, Case and maybe type
closureExpr (XLet bindings expr) = XLet bindings expr
closureExpr (XCase match binder alts) = XCase match binder alts
closureExpr (XType ty) = XType ty
closureExpr (XCoercion) = XCoercion
closureExpr (XXExpr extension) = XXExpr extension

-- Heuristic that pairs up top-level bindings for diffing complete modules
-- Steps:
-- * Find bindings whose (non-unique) binder names appear on each side exactly once, pair those.
-- * Group bindings that share a binder name. For each group:
-- * Find bindings whose Type structure appears on each side exactly once, pair those.
-- * Find bindings whose Term structure appears on each side exactly once, pair those.
-- * ?
-- * Profit
data Pairing a
  = Both a a -- appears on both sides
  | OnlyLeft a   -- appears only on the left side
  | OnlyRight a  -- appears only on the right side
  deriving (Show)

findPairings :: [XBinding UD] -> [XBinding UD] -> [Pairing (XBinding UD)]
findPairings ls rs =
  foldl go [] allNames
  where
    go acc name
      | length ls' == 1 && length rs' == 1 = acc ++ [Both (head ls') (head rs')]
      | otherwise                          = acc ++ foldl go' [] allTypes
      where
        ls' = filter ((== name) . xBinderName . xb) ls
        rs' = filter ((== name) . xBinderName . xb) rs

        lTypes = map (xBinderType . xb) ls'
        rTypes = map (xBinderType . xb) rs'
        allTypes = nub $ union lTypes rTypes

        go' acc ty
          | length ls'' == 1 && length rs'' == 1 = acc ++ [Both (head ls'') (head rs'')]
          | otherwise                          = acc ++ map OnlyLeft ls'' ++ map OnlyRight rs''
          where
            ls'' = filter ((== ty) . xBinderType . xb) ls'
            rs'' = filter ((== ty) . xBinderType . xb) rs'

    lNames = map (xBinderName . xb) ls
    rNames = map (xBinderName . xb) rs
    allNames = nub $ union lNames rNames

    xb (XBinding binder _) = binder
