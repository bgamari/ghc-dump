{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- Contains a lot of code from GhcDump.Ast
module CoreDiff.PrettyPrint where

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Ratio
import qualified Data.Text as T
import Data.Void
import GhcDump.Ast
-- Extension of functional pretty-printer presented in
-- https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
-- Provides means to output ANSI color codes
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import CoreDiff.XAst

data PprOptions = PprOptions
  { pprShowUniques :: Bool
  , pprShowIdInfo :: Bool
  }

pprDefaultOpts = PprOptions
  { pprShowUniques = True
  , pprShowIdInfo = True
  }


class PprOpts a where
  ppr :: a -> Reader PprOptions Doc


instance ForAllExtensions PprOpts a => PprOpts (XBinding a) where
  {- Implementation as in ghc-dump-util (misses case for binder-hole
  ppr (XBinding binder@XBinder{} expr) = do
    typeSig <- pprTypeSig binder
    idInfo <- pprIdInfo $ xBinderIdInfo binder
    binderDoc <- ppr binder
    exprDoc <- ppr expr
    let assignment = hang' (binderDoc <+> equals) 2 exprDoc

    return $ typeSig <$$> idInfo <$$> assignment

  ppr (XBinding binder@XTyBinder{} expr) = do
    binderDoc <- ppr binder
    exprDoc <- ppr expr
    let assignment = hang' (binderDoc <+> equals) 2 (exprDoc)

    return assignment
  -}

  ppr (XBinding binder expr) = do
    binderDoc <- ppr binder
    expr <- ppr expr
    return $ hang' (binderDoc <+> equals) 2 expr

  ppr (XXBinding extension) = ppr extension


pprTypeSig :: ForAllExtensions PprOpts a => XBinder a -> Reader PprOptions Doc
pprTypeSig binder@XBinder{} = do
  binderDoc <- ppr binder
  typeDoc <- ppr $ xBinderType binder

  return $ binderDoc <+> dcolon <+> align typeDoc


instance ForAllExtensions PprOpts a => PprOpts (XBinder a) where
  ppr = pprBinder


pprBinder :: ForAllExtensions PprOpts a => XBinder a -> Reader PprOptions Doc
pprBinder (XXBinder extension) = ppr extension
pprBinder (binder@XTyBinder{}) = pprBinderName binder
pprBinder (binder@XBinder{})   = do
  opts <- ask
  if not $ pprShowIdInfo opts then
    pprBinderName binder
  else do
    nameDoc <- pprBinderName binder
    tyDoc <- ppr $ xBinderType binder
    infoDoc <- pprIdInfo $ xBinderIdInfo binder
    return $ nameDoc <+> infoDoc <+> dcolon <+> tyDoc


instance ForAllExtensions PprOpts a => PprOpts (XExpr a) where
  ppr = pprExpr False

pprExpr :: ForAllExtensions PprOpts a => Bool -> XExpr a -> Reader PprOptions Doc
pprExpr _ (XVar binder) = local dontShowIdInfo $ ppr binder
pprExpr _ (XVarGlobal extName) = return $ pprExtName extName
pprExpr _ (XLit lit) = return $ pprLit lit
pprExpr _ (XCoercion) = return "CO"
pprExpr _ (XXExpr extension) = ppr extension
pprExpr parens expr = maybeParens parens <$> pprExpr' expr

pprExpr' expr@(XApp{}) = do
  fDoc <- pprExpr True f
  argDocs <- mapM (pprExpr True) args

  return $ hang' fDoc 2 (sep argDocs)
  where
    (f, args) = collectArguments expr

pprExpr' expr@(XTyLam{}) = do
  paramDocs <- mapM ppr params
  bodyDoc <- pprExpr False body

  return $ hang' ("\\@" <+> sep paramDocs <+> "->") 2 bodyDoc
  where (params, body) = collectTyBinders expr

pprExpr' expr@(XLam{}) = do
  paramDocs <- mapM ppr params
  bodyDoc <- pprExpr False body

  return $ hang' ("\\" <+> sep paramDocs <+> "->") 2 bodyDoc
  where (params, body) = collectBinders expr

pprExpr' (XLet bindings body) = do
  bindingDocs <- mapM ppr bindings
  bodyDoc <- pprExpr False body
  return $ "let" <+> (align $ vcat $ bindingDocs) <$$> "in" <+> align bodyDoc

pprExpr' (XCase match binder alts) = do
  matchDoc <- pprExpr False match
  binderDoc <- ppr binder
  altDocs <- mapM ppr alts

  return $ sep [ sep [ "case" <+> matchDoc
                     , "of" <+> binderDoc <+> "{"
                     ]
               , indent 2 $ vcat $ altDocs
               , "}"
               ]

pprExpr' (XType ty) = do
  tyDoc <- ppr ty

  return $ "TYPE:" <+> tyDoc


instance ForAllExtensions PprOpts a => PprOpts (XAlt a) where
  ppr (XAlt con binders rhs) = do
    binderDocs <- mapM ppr binders
    rhsDoc <- pprExpr False rhs
    
    return $ hang' (hsep (pprAltCon con : binderDocs) <+> "->") 2 rhsDoc

  ppr (XXAlt extension) = ppr extension


instance ForAllExtensions PprOpts a => PprOpts (XType a) where
  ppr = pprType' TopPrec


data TyPrec
  = TopPrec
  | FunPrec
  | TyOpPrec
  | TyConPrec
  deriving (Eq, Ord)

pprType' :: ForAllExtensions PprOpts a => TyPrec -> (XType a) -> Reader PprOptions Doc
pprType' _    (XVarTy binder) = ppr binder
pprType' prec t@XFunTy{} = do
  funTyDocs <- mapM (pprType' FunPrec) funTys
  return $ maybeParens (prec >= FunPrec) $ sep $ punctuate " ->" funTyDocs
  where funTys = collectFunTys t
-- TODO: special types like [], (,), etc.
pprType' _    (XTyConApp tyCon []) = return $ pprTyCon tyCon
pprType' prec (XTyConApp tyCon tys) = do
  tyDocs <- mapM (pprType' TyConPrec) tys
  return $ maybeParens (prec >= FunPrec) $ pprTyCon tyCon <+> hsep tyDocs
pprType' prec (XAppTy f x) = do
  fDoc <- pprType' TyConPrec f
  xDoc <- pprType' TyConPrec x
  return $ maybeParens (prec >= TyConPrec) $ fDoc <+> xDoc
pprType' prec t@XForAllTy{} = do
  binderDocs <- mapM ppr binders
  tyDoc <- ppr ty
  return $ maybeParens (prec >= TyOpPrec) $ "forall" <+> hsep binderDocs <> dot <+> tyDoc
  where
    (binders, ty) = collectForAlls t
pprType' _ (XXType extension) = ppr extension


-- pretty-printing of extensions

instance PprOpts Void where
  ppr _ = error "Oh dear!"


-- todo: special instances
instance PprOpts x => PprOpts (Change x) where
  ppr (Change (lhs, rhs)) = do
    lhsDoc <- ppr lhs
    rhsDoc <- ppr rhs
    -- TODO: nice indentation
    return $ "#" <> align (vsep [ "( " <> red lhsDoc, "/ " <> green rhsDoc, ")" ])

-- terminals

pprExtName extName@ExternalName{} =
  modName <> dot <> varName
  where
    modName = pretty $ getModuleName $ externalModuleName extName
    varName = pretty $ externalName extName
pprExtName ForeignCall = "<foreign>"

pprLit (MachChar c) = squotes $ pretty c
pprLit (MachStr s) = dquotes $ text $ BS.unpack s
pprLit (MachNullAddr) = "nullAddr#"
pprLit (MachInt i) = pretty i <> "#"
pprLit (MachInt64 i) = pretty i <> "#"
pprLit (MachWord w) = pretty w <> "#"
pprLit (MachWord64 w) = pretty w <> "##"
pprLit (MachFloat f) = "FLOAT" <> parens (pprRational f)
pprLit (MachDouble d) = "DOUBLE" <> parens (pprRational d)
pprLit (MachLabel l) = "LABEL" <> parens (pretty l)
pprLit (LitInteger i) = pretty i

pprRational r = pretty (numerator r) <> "/" <> pretty (denominator r)

pprAltCon (AltDataCon t) = pretty t  
pprAltCon (AltLit l) = pprLit l
pprAltCon (AltDefault) = "DEFAULT"

pprBinderName binder = do
  opts <- ask
  if pprShowUniques opts then
    return $ pretty (xBinderName binder) <> "_" <> pretty (xBinderId binder)
  else
    return $ pretty $ xBinderName binder

pprIdInfo :: IdInfo Binder Binder -> Reader PprOptions Doc
pprIdInfo idi = return $ brackets $ align $ sep $ punctuate ", " $ catMaybes $
  -- TODO: don't show empty fields
  [ toMaybe (idiArity idi /= 0)               $ "arity=" <> pretty (idiArity idi)
  , toMaybe (idiInlinePragma idi /= "")       $ "inline=" <> pretty (idiInlinePragma idi)
  , Just $ "occ=" <> pretty (idiOccInfo idi)
  , toMaybe (idiStrictnessSig idi /= "")      $ "str=" <> pretty (idiStrictnessSig idi)
  , toMaybe (idiDemandSig idi /= "")          $ "dmd=" <> pretty (idiDemandSig idi)
  , toMaybe (idiCallArity idi /= 0)           $ "call-arity=" <> pretty (idiCallArity idi)
  , toMaybe (idiUnfolding idi /= NoUnfolding) $ "unfolding=" <> pretty (idiUnfolding idi)
  , toMaybe (idiIsOneShot idi)                $ "one-shot"
  ]
  where toMaybe True x = Just x
        toMaybe _    _ = Nothing

instance Pretty T.Text where
  pretty = text . T.unpack

instance Pretty BinderId where
  pretty (BinderId (Unique c i)) = pretty c <> pretty i

instance Pretty (Unfolding Binder Binder) where
  pretty NoUnfolding = "NoUnfolding"
  pretty BootUnfolding = "BootUnfolding"
  pretty OtherCon{} = "OtherCon"
  pretty DFunUnfolding = "DFunUnfolding"
  pretty CoreUnfolding{} = "CoreUnf{..}"

instance Pretty OccInfo where
    pretty OccManyOccs = "Many"
    pretty OccDead = "Dead"
    pretty OccOneOcc = "One"
    pretty (OccLoopBreaker strong) =
      if strong then "Strong Loopbrk" else "Weak Loopbrk"

pprTyCon (TyCon t _) = pretty t

-- some helpers

dontShowIdInfo opts = opts { pprShowIdInfo = False }

dcolon :: Doc
dcolon = "::"

maybeParens True = parens
maybeParens False = id

hang' l i r = hang i $ sep [l, r]

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

collectFunTys :: XType a -> [XType a]
collectFunTys = go []
  where go acc (XFunTy l r) = go (l : acc) r
        go acc ty          = reverse acc ++ [ty] -- <=> reverse (ty : acc)

collectForAlls :: XType a -> ([XBinder a], XType a)
collectForAlls = go []
  where go acc (XForAllTy binder ty) = go (binder : acc) ty
        go acc ty                   = (reverse acc, ty)
