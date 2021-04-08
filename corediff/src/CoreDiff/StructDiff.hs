{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

module CoreDiff.StructDiff where

import Unsafe.Coerce

import CoreDiff.XAst

diff a b = mend $ gcp a b

class Diffable (a :: Variant -> *) where
  gcp  :: a UD -> a UD -> a Diff
  mend :: a Diff -> a Diff


instance Diffable XBinding where
  gcp (XBinding binder expr) (XBinding binder' expr')
    -- if binder metadata and types mismatch, mark as change even if their uniques are equal
    | binder ~/~ binder' =
      XBinding (XXBinder (Change (binder, binder'))) (gcp expr expr')
    | otherwise =
      XBinding (gcp binder binder') (gcp expr expr')
    where
      (XBinder   name unique _ _ ty metadata) ~/~ (XBinder   name' unique' _ _ ty' metadata') =
        ty /= ty' || metadata /= metadata'
      (XTyBinder name unique _ _ kind)        ~/~ (XTyBinder name' unique' _ _ kind')         =
        kind /= kind'
      _ ~/~ _ = False

  mend (XBinding binder expr) =
    case XBinding (mend binder) (mend expr) of
      XBinding (XXBinder (Change (binder, binder'))) (XXExpr (Change (expr, expr'))) ->
        XXBinding $ Change
          ( XBinding binder  expr
          , XBinding binder' expr'
          )
      mended ->
        mended

instance Diffable XBinder where
  gcp binder binder'
    | binder == binder' = unsafeCoerce binder
    | otherwise         = XXBinder $ Change (binder, binder')
  
  mend binder = binder

instance Diffable XExpr where
  gcp (XVar binder) (XVar binder') =
    XVar $ gcp binder binder'
  gcp (XApp f x) (XApp f' x') =
    XApp (gcp f f') (gcp x x')
  gcp (XTyLam binder expr) (XTyLam binder' expr') =
    XTyLam (gcp binder binder') (gcp expr expr')
  gcp (XLam   binder expr) (XLam   binder' expr') =
    XLam   (gcp binder binder') (gcp expr expr')
  gcp (XLet [binding] expr) (XLet [binding'] expr') =
    XLet [gcp binding binding'] $ gcp expr expr'
  gcp (XCase scrut binder alts) (XCase scrut' binder' alts')
    | length alts /= length alts' = error "Mismatched number of alternatives"
    | otherwise =
      XCase (gcp scrut scrut') (gcp binder binder') (zipWith gcp alts alts')
  -- TODO: types
  gcp expr expr'
    | expr == expr' = unsafeCoerce expr
    | otherwise     = XXExpr $ Change (expr, expr')

  mend (XVar binder) =
    case XVar $ mend binder of
      XVar (XXBinder (Change (binder, binder'))) ->
        XXExpr $ Change (XVar binder, XVar binder')
      mended ->
        mended
  mend (XApp f x) =
    case XApp (mend f) (mend x) of
      XApp (XXExpr (Change (f, f'))) (XXExpr (Change (x, x'))) ->
        XXExpr $ Change (XApp f x, XApp f' x')
      mended ->
        mended
  mend (XTyLam binder expr) =
    case XTyLam (mend binder) (mend expr) of
      XTyLam (XXBinder (Change (binder, binder'))) (XXExpr (Change (expr, expr'))) ->
        XXExpr $ Change (XTyLam binder expr, XTyLam binder' expr')
      mended ->
        mended
  mend (XLam binder expr) =
    case XLam (mend binder) (mend expr) of
      XLam (XXBinder (Change (binder, binder'))) (XXExpr (Change (expr, expr'))) ->
        XXExpr $ Change (XLam binder expr, XLam binder' expr')
      mended ->
        mended
  mend (XLet [binding] expr) =
    case XLet [mend binding] (mend expr) of
      XLet [XXBinding (Change (binding, binding'))] (XXExpr (Change (expr, expr'))) ->
        XXExpr $ Change (XLet [binding] expr, XLet [binding'] expr')
      mended ->
        mended
  -- TODO: let, case, types

  mend expr = expr

instance Diffable XAlt where
  gcp alt@(XAlt altCon binders rhs) alt'@(XAlt altCon' binders' rhs')
    | length binders /= length binders' = error "Mismatched number of alternative binders"
    | altCon == altCon' =
      XAlt altCon (zipWith gcp binders binders') $ gcp rhs rhs'
    | otherwise =
      XXAlt $ Change (alt, alt')

  -- TODO: mend
  mend alt = alt
