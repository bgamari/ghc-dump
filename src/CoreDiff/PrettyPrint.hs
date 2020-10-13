{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module CoreDiff.PrettyPrint where

import Control.Monad.Trans.Reader
import Data.Maybe
import qualified Data.Text as T
import GhcDump.Ast (IdScope(..), OccInfo(..))
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))


import CoreDiff.XAst


text' = text . T.unpack


-- | Options for pretty-printing Core terms.
data PprOpts = PprOpts
  { pprOptsDisplayUniques :: Bool -- ^ Whether uniques should be printed.
  , pprOptsDisplayMeta    :: Bool -- ^ Whether metadata should be printed.
  , pprOptsLongBindings   :: Bool -- ^ Whether bindings should be printed with a separate signature.
  }

-- | The default options for pretty-printing Core terms.
pprOptsDefault = PprOpts True True True

-- | Stuff that is pretty-printable in respect to some @PprOpts@.
class PprWithOpts a where
  pprWithOpts :: a -> Reader PprOpts Doc


instance PprWithOpts XModule where
  pprWithOpts mod = do
    bindingDocs <- mapM pprWithOpts $ xModuleBindings mod

    return $ vsep $
      [ text' $ "Phase: " <> xModulePhase mod
      , text' $ "module " <> xModuleName mod <> " where"
      ] ++ bindingDocs


instance PprWithOpts (XBinding a) where
  pprWithOpts (XXBinding _) = error "stub"
  pprWithOpts binding = do
    opts <- ask
    if pprOptsLongBindings opts then
      pprLongBinding binding
    else
      pprShortBinding binding
    where
      pprLongBinding :: XBinding a -> Reader PprOpts Doc
      pprLongBinding (XBinding binder expr) = do
        sigDoc <- pprSignature binder
        binderNameDoc <- pprBinderName binder
        exprDoc <- pprWithOpts expr

        return $ vsep [ sigDoc, hang' (binderNameDoc <+> equals) 2 exprDoc ]

      pprSignature :: XBinder a -> Reader PprOpts Doc
      pprSignature binder = do
        opts <- ask
        metaDoc <- pprBinderMeta $ xBinderMeta binder
        nameDoc <- pprBinderName binder
        typeDoc <- pprWithOpts $ xBinderTypeOrKind binder

        return $ vsep $ catMaybes
          [ toMaybe (pprOptsDisplayMeta opts) metaDoc
          , Just $ hang' (nameDoc <+> dcolon) 2 typeDoc
          ]

      pprShortBinding :: XBinding a -> Reader PprOpts Doc
      pprShortBinding (XBinding binder expr) = do
        opts <- ask
        binderNameDoc <- pprBinderName binder
        metaDoc <- pprBinderMeta $ xBinderMeta binder
        typeDoc <- pprWithOpts $ xBinderTypeOrKind binder
        exprDoc <- pprWithOpts expr

        return $ hang' (hsep $ catMaybes [ Just binderNameDoc, toMaybe (pprOptsDisplayMeta opts) metaDoc, Just dcolon, Just typeDoc, Just equals]) 2 exprDoc


instance PprWithOpts (XBinder a) where
  pprWithOpts (XXBinder _) = error "stub"
  pprWithOpts binder = pprBinderName binder


instance PprWithOpts (XType a) where
  pprWithOpts (XXType _) = error "stub"
  pprWithOpts ty = pprType TopPrec ty
  
data TyPrec = TopPrec | FunPrec | TyOpPrec | TyConPrec
  deriving (Eq, Ord)

pprType :: TyPrec -> XType a -> Reader PprOpts Doc
pprType _    (XVarTy binder) = pprBinderName binder
pprType prec t@XFunTy{} = do
  funTyDocs <- mapM (pprType FunPrec) funTys
  return $ parensIf (prec >= FunPrec) $ sep $ punctuate " ->" funTyDocs
    where funTys = collectFunTys t
-- TODO: special tycons like [], (,), etc.
pprType _    (XTyConApp tyCon []) = return $ pprTyCon tyCon
pprType prec (XTyConApp tyCon tys) = do
  tyDocs <- mapM (pprType TyConPrec) tys
  return $ parensIf (prec >= FunPrec) $ pprTyCon tyCon <+> hsep tyDocs
pprType prec (XAppTy f x) = do
  fDoc <- pprType TyConPrec f
  xDoc <- pprType TyConPrec x
  return $ parensIf (prec >= TyConPrec) $ fDoc <+> xDoc
pprType prec t@XForAllTy{} = do
  binderDocs <- mapM pprBinderName binders
  tyDoc <- pprType TopPrec ty
  return $ parensIf (prec >= TyOpPrec) $ "forall" <+> hsep binderDocs <> dot <+> tyDoc
  where
  (binders, ty) = collectForAlls t


instance PprWithOpts (XExpr a) where
  pprWithOpts (XXExpr _) = error "stub"
  pprWithOpts _ = return $ text "tbd"

instance PprWithOpts (XAlt a) where
  pprWithOpts (XXAlt _) = error "stub"

-- Terminals

-- | Takes care of printing binders with or without uniques.
pprBinderName binder = do
  opts <- ask
  if pprOptsDisplayUniques opts then
    return $ text' $ xBinderName binder <> "_" <> T.pack (show $ xBinderId binder)
  else
    return $ text' $ xBinderName binder

pprBinderMeta :: XBinderMeta -> Reader PprOpts Doc
pprBinderMeta meta = do
  occInfoDoc <- pprOccInfo $ xbmOccInfo meta

  return $ brackets $ align $ sep $ punctuate "," $ catMaybes $
    [ Just $ pprScope $ xbmScope meta
    , toMaybe (xbmArity meta /= 0)               $ "arity=" <> pretty (xbmArity meta)
    , toMaybe (xbmInlinePragma meta /= "")       $ "inline=" <> text' (xbmInlinePragma meta)
    , Just $ "occ=" <> occInfoDoc
    , toMaybe (xbmStrictnessSig meta /= "")      $ "str=" <> text' (xbmStrictnessSig meta)
    , toMaybe (xbmDemandSig meta /= "")          $ "dmd=" <> text' (xbmDemandSig meta)
    , toMaybe (xbmCpr meta /= "")                $ "cpr=" <> text' (xbmCpr meta)
    , toMaybe (xbmCallArity meta /= 0)           $ "call-arity=" <> pretty (xbmCallArity meta)
    , toMaybe (xbmIsOneShot meta)                $ "one-shot"
    ]
  where
    pprScope GlobalId = "GblId"
    pprScope LocalIdX = "LclIdX"
    pprScope LocalId  = "LclId"

    pprOccInfo OccManyOccs = return "Many"
    pprOccInfo OccDead = return "Dead"
    pprOccInfo OccOneOcc = return "One"
    pprOccInfo (OccLoopBreaker isStrong) =
      return $ (if isStrong then "Strong" else "Weak") <+> "Loopbrk"

pprTyCon (TyCon' t) = text' t

-- Helper functions

dcolon = text "::"

-- | Copied from @GhcDump.Pretty@.
hang' left indent right = hang indent $ sep [ left, right ]

toMaybe True  x = Just x
toMaybe False _ = Nothing

parensIf True  = parens
parensIf False = id

collectFunTys :: XType a -> [XType a]
collectFunTys = go []
  where go acc (XFunTy l r) = go (l : acc) r
        go acc ty           = reverse $ ty : acc

collectForAlls :: XType a -> ([XBinder a], XType a)
collectForAlls = go []
  where go acc (XForAllTy binder ty) = go (binder : acc) ty
        go acc ty                    = (reverse acc, ty)
