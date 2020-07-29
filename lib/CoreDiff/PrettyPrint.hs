{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Contains a lot of code from GhcDump.Ast
module CoreDiff.PrettyPrint where

import Control.Monad.Reader
import Data.Ratio
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import GhcDump.Ast
-- Extension of functional pretty-printer presented in
-- https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
-- Provides means to output ANSI color codes
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import CoreDiff.XAst

data PprOptions = PprOptions
  { pprShowUniques :: Bool
  }

pprDefaultOpts = PprOptions
  { pprShowUniques = True
  }


class PprOpts a where
  ppr :: a -> Reader PprOptions Doc


instance PprOpts (XBinding a) where
  ppr (XBinding binder@(XBinder (Bndr b@Binder{})) expr) = do
    typeSig <- pprTypeSig binder
    idInfo <- pprIdInfo $ binderIdInfo b
    binderDoc <- ppr binder
    exprDoc <- ppr expr
    let assignment = hang' (binderDoc <+> equals) 2 exprDoc

    return $ typeSig <$$> idInfo <$$> assignment

  ppr (XBinding binder@(XBinder (Bndr TyBinder{})) expr) = do
    binderDoc <- ppr binder
    exprDoc <- ppr expr
    let assignment = hang' (binderDoc <+> equals) 2 (exprDoc)

    return assignment

  -- TODO: XXBinding

pprIdInfo :: IdInfo Binder Binder -> Reader PprOptions Doc
pprIdInfo idi = return "TODO"
-- TODO


pprTypeSig :: XBinder a -> Reader PprOptions Doc
pprTypeSig b@(XBinder (Bndr binder)) = do
  binderDoc <- ppr b
  typeDoc <- ppr $ binderType binder -- TODO: move somewhere nice.

  return $ binderDoc <+> dcolon <+> align typeDoc


instance PprOpts (XBinder a) where
  ppr (XBinder binder) = pprBinder binder
  -- TODO: pprBinder (XXBinder ...

pprBinder :: Binder -> Reader PprOptions Doc
pprBinder binder@(Bndr b) = do
  opts <- ask
  if pprShowUniques opts then
    return $ text $ T.unpack $ binderUniqueName binder
  else
    return $ text $ T.unpack $ binderName b


instance PprOpts (XExpr a) where
  ppr = pprExpr False

pprExpr :: Bool -> XExpr a -> Reader PprOptions Doc
pprExpr _ (XVar binder) = ppr binder
pprExpr _ (XVarGlobal extName) = return $ pprExtName extName
pprExpr _ (XLit lit) = return $ pprLit lit
pprExpr _ (XCoercion) = return "CO"
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


instance PprOpts (XAlt a) where
  ppr (XAlt con binders rhs) = do
    binderDocs <- mapM ppr binders
    rhsDoc <- pprExpr False rhs
    
    return $ hang' (hsep (pprAltCon con : binderDocs) <+> "->") 2 rhsDoc


pprExtName extName@ExternalName{} =
  modName <> dot <> varName
  where
    modName = text $ T.unpack $ getModuleName $ externalModuleName extName
    varName = text $ T.unpack $ externalName extName
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
pprLit (MachLabel l) = "LABEL" <> parens (text $ T.unpack l)
pprLit (LitInteger i) = pretty i

pprRational r = pretty (numerator r) <> "/" <> pretty (denominator r)

pprAltCon (AltDataCon t) = text $ T.unpack t  
pprAltCon (AltLit l) = pprLit l
pprAltCon (AltDefault) = "DEFAULT"


instance PprOpts Type where
  ppr = pprType' TopPrec


data TyPrec
  = TopPrec
  | FunPrec
  | TyOpPrec
  | TyConPrec
  deriving (Eq, Ord)

pprType' :: TyPrec -> Type -> Reader PprOptions Doc
pprType' _    (VarTy binder) = pprBinder binder
pprType' prec t@FunTy{} = do
  funTyDocs <- mapM (pprType' FunPrec) funTys
  return $ maybeParens (prec >= FunPrec) $ sep $ punctuate " ->" funTyDocs
  where funTys = collectFunTys t
-- TODO: special types like [], (,), etc.
pprType' _    (TyConApp tyCon []) = return $ pprTyCon tyCon
pprType' prec (TyConApp tyCon tys) = do
  tyDocs <- mapM (pprType' TyConPrec) tys
  return $ maybeParens (prec >= FunPrec) $ pprTyCon tyCon <+> hsep tyDocs
pprType' prec (AppTy f x) = do
  fDoc <- pprType' TyConPrec f
  xDoc <- pprType' TyConPrec x
  return $ maybeParens (prec >= TyConPrec) $ fDoc <+> xDoc
pprType' prec t@ForAllTy{} = do
  let binderDocs = map (text . show) binders
  tyDoc <- ppr ty
  return $ maybeParens (prec >= TyOpPrec) $ "forall" <+> hsep binderDocs <> dot <+> tyDoc
  where
    (binders, ty) = collectForAlls t

pprType' _ _ = return "TODO"
-- TODO

pprTyCon (TyCon t _) = text $ T.unpack t

-- some helpers

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

collectFunTys :: Type -> [Type]
collectFunTys = go []
  where go acc (FunTy l r) = go (l : acc) r
        go acc ty          = reverse acc ++ [ty] -- <=> reverse (ty : acc)

collectForAlls :: Type -> ([Binder], Type)
collectForAlls = go []
  where go acc (ForAllTy binder ty) = go (binder : acc) ty
        go acc ty                   = (reverse acc, ty)
