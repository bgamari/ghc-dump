{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module CoreDiff.PrettyPrint where

import Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import Data.Ratio
import qualified Data.Text as T
import Data.Void
import GhcDump.Ast (IdScope(..), OccInfo(..), Lit(..), AltCon(..))
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

    return $ vsep $ intersperse empty $
      [ text' $ "Phase: " <> xModulePhase mod
      , text' $ "module " <> xModuleName mod <> " where"
      ] ++ bindingDocs


instance ForAllExtensions PprWithOpts a => PprWithOpts (XBinding a) where
  -- big rule of thumb: each global function that uses ppr-functions other that pprWithOpts, which is guaranteed to handle extensions, must handle extensions itself.
  -- Also true for any functions that pattern-match. basically all PprWithOpts instances and then some.
  pprWithOpts (XXBinding extension) = pprWithOpts extension
  pprWithOpts binding = do
    opts <- ask
    if pprOptsLongBindings opts then
      pprLongBinding binding
    else
      pprShortBinding binding
    where
      pprLongBinding :: ForAllExtensions PprWithOpts a => XBinding a -> Reader PprOpts Doc
      pprLongBinding (XBinding binder expr) = do
        sigDoc <- pprSignature binder
        binderNameDoc <- pprWithOpts binder
        exprDoc <- pprWithOpts expr

        return $ vsep [ sigDoc, hang' (binderNameDoc <+> equals) 2 exprDoc ]

      pprSignature :: ForAllExtensions PprWithOpts a => XBinder a -> Reader PprOpts Doc
      pprSignature binder = do
        opts <- ask
        metaDoc <- pprBinderMeta $ xBinderMeta binder
        nameDoc <- pprWithOpts binder
        typeDoc <- pprWithOpts $ xBinderTypeOrKind binder

        return $ vsep $ catMaybes
          [ toMaybe (pprOptsDisplayMeta opts) metaDoc
          , Just $ hang' (nameDoc <+> dcolon) 2 typeDoc
          ]

      pprShortBinding :: ForAllExtensions PprWithOpts a => XBinding a -> Reader PprOpts Doc
      pprShortBinding (XBinding binder expr) = do
        binderDoc <- pprWithOpts binder
        exprDoc <- pprWithOpts expr

        return $ hang' (binderDoc <+> equals) 2 exprDoc


-- | Takes care of printing binders with or without uniques.
instance ForAllExtensions PprWithOpts a => PprWithOpts (XBinder a) where
  pprWithOpts (XXBinder extension) = pprWithOpts extension
  pprWithOpts binder = do
    opts <- ask
    if pprOptsDisplayUniques opts then
      return $ text' $ xBinderName binder <> "_"
                    <> T.pack (show $ xBinderId binder)
    else
      return $ text' $ xBinderName binder


instance ForAllExtensions PprWithOpts a => PprWithOpts (XType a) where
  -- extension case is handled in pprType
  pprWithOpts ty = pprType TopPrec ty
  
data TyPrec = TopPrec | FunPrec | TyOpPrec | TyConPrec
  deriving (Eq, Ord)

pprType :: ForAllExtensions PprWithOpts a => TyPrec -> XType a -> Reader PprOpts Doc
pprType _    (XXType extension) = pprWithOpts extension
pprType _    (XVarTy binder) = pprWithOpts binder
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
  binderDocs <- mapM pprWithOpts binders
  tyDoc <- pprType TopPrec ty
  return $ parensIf (prec >= TyOpPrec) $ "forall" <+> hsep binderDocs <> dot <+> tyDoc
  where
  (binders, ty) = collectForAlls t


instance ForAllExtensions PprWithOpts a => PprWithOpts (XExpr a) where
  -- extension case is handled in pprExpr
  pprWithOpts = pprExpr False

-- | Expressions can be printed with or without parentheses around them.
-- @pprExpr@ prints terminal expressions without parens regardless.
pprExpr :: ForAllExtensions PprWithOpts a => Bool -> XExpr a -> Reader PprOpts Doc
pprExpr _ (XXExpr extension)   = pprWithOpts extension
pprExpr _ (XVar binder)        = pprWithOpts binder
pprExpr _ (XVarGlobal extName) = return $ pprExtName extName
pprExpr _ (XLit lit)           = pprLit lit
pprExpr _ (XCoercion)          = return "CO"
pprExpr parens (XType ty) = do
  tyDoc <- pprType (if parens then TyConPrec else TopPrec) ty
  return $ "@" <+> tyDoc
pprExpr parens expr = parensIf parens <$> pprExpr' expr
  where
    pprExpr' expr@(XApp{}) = do
      fDoc <- pprExpr True f
      argDocs <- mapM (pprExpr True) args
      return $ hang' fDoc 2 (sep argDocs)
      where
        (f, args) = collectArguments expr

    pprExpr' expr@(XTyLam{}) = do
      paramDocs <- mapM pprWithOpts params
      bodyDoc <- pprWithOpts body
      return $ hang' ("\\@" <+> sep paramDocs <+> "->") 2 bodyDoc
      where (params, body) = collectTyBinders expr

    pprExpr' expr@(XLam{}) = do
      paramDocs <- mapM pprWithOpts params
      bodyDoc <- pprWithOpts body
      return $ hang' ("\\" <+> sep paramDocs <+> "->") 2 bodyDoc
      where (params, body) = collectBinders expr

    pprExpr' (XLet bindings body) = do
      bindingDocs <- mapM pprWithOpts bindings
      bodyDoc <- pprWithOpts body
      return $ "let" <+> (align $ vcat $ bindingDocs) <$$> "in" <+> align bodyDoc

    pprExpr' (XCase match binder alts) = do
      matchDoc <- pprWithOpts match
      binderDoc <- pprWithOpts binder
      altDocs <- mapM pprWithOpts alts

      return $ sep
        [ sep [ "case" <+> matchDoc, "of" <+> binderDoc <+> "{" ]
        , indent 2 $ vcat $ altDocs
        , "}"
        ]

instance ForAllExtensions PprWithOpts a => PprWithOpts (XAlt a) where
  pprWithOpts (XXAlt extension) = pprWithOpts extension
  pprWithOpts (XAlt con binders rhs) = do
    altConDoc <- pprAltCon con
    binderDocs <- mapM pprWithOpts binders
    rhsDoc <- pprWithOpts rhs
    return $ hang' (altConDoc <+> sep binderDocs <+> "->") 2 rhsDoc

-- Pretty-printing of extensions


instance PprWithOpts Void where
  pprWithOpts _ = error "Something went terribly wrong! There is nothing to print."

instance PprWithOpts a => PprWithOpts (Change a) where
  pprWithOpts (Change (lhs, rhs)) = do
    lhsDoc <- pprWithOpts lhs
    rhsDoc <- pprWithOpts rhs
    return $ "#( " <> lhsDoc <> ", " <> rhsDoc <> " )"

-- Terminals

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

pprLit :: Lit -> Reader PprOpts Doc
pprLit = return . pprLit'
  where
    pprLit' (MachChar c) = squotes $ pretty c
    pprLit' (MachStr s) = dquotes $ text $ BS.unpack s
    pprLit' (MachNullAddr) = "nullAddr#"
    pprLit' (MachInt i) = pretty i <> "#"
    pprLit' (MachInt64 i) = pretty i <> "#"
    pprLit' (MachWord w) = pretty w <> "#"
    pprLit' (MachWord64 w) = pretty w <> "##"
    pprLit' (MachFloat f) = "FLOAT" <> parens (pprRational f)
    pprLit' (MachDouble d) = "DOUBLE" <> parens (pprRational d)
    pprLit' (MachLabel l) = "LABEL" <> parens (text' l)
    pprLit' (LitInteger i) = pretty i

    pprRational r = pretty (numerator r) <> "/" <> pretty (denominator r)

pprTyCon (TyCon' t) = text' t

pprAltCon (AltDataCon name) = return $ text' name
pprAltCon (AltLit lit)      = pprLit lit
pprAltCon (AltDefault)      = return "__DEFAULT"

pprExtName (ExternalName' modName name) = text' modName <> dot <> text' name
pprExtName ForeignCall' = "<foreign>"

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

collectArguments :: XExpr a -> (XExpr a, [XExpr a])
collectArguments = go []
  where go acc (XApp f x) = go (x : acc) f
        go acc expr       = (expr, acc)

collectTyBinders :: XExpr a -> ([XBinder a], XExpr a)
collectTyBinders = go []
  where go acc (XTyLam p b) = go (p : acc) b
        go acc expr       = (reverse acc, expr)

collectBinders :: XExpr a -> ([XBinder a], XExpr a)
collectBinders = go []
  where go acc (XLam p b) = go (p : acc) b
        go acc expr       = (reverse acc, expr)
