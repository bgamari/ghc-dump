{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module GhcDump.Pretty
    ( Pretty(..)
    , module GhcDump.Pretty
    ) where

import GhcDump.Ast
import GhcDump.Util
import qualified Data.Text as T
import Text.PrettyPrint.ANSI.Leijen

instance Pretty ExternalName where
    pretty n = pretty (externalModuleName n) <> "." <> text (T.unpack $ externalName n)

instance Pretty ModuleName where
    pretty = text . T.unpack . getModuleName

instance Pretty Unique where
    pretty (Unique c n) = char c <> int n

instance Pretty Binder where
    pretty (Bndr b) = text $ T.unpack (binderName b)

instance Pretty TyCon where
    pretty (TyCon t _) = text $ T.unpack t

instance Pretty Lit where
    pretty SomeLit = text "LIT"

instance Pretty CoreStats where
    pretty c = "SIZE"<>braces (hsep [ "terms="<>int (cs_terms c)
                                    , "types="<>int (cs_types c)
                                    , "cos="<>int (cs_coercions c)
                                    , "vbinds="<>int (cs_val_binds c)
                                    , "jbinds="<>int (cs_join_binds c)
                                    ])

data TyPrec   -- See Note [Precedence in types] in TyCoRep.hs
  = TopPrec         -- No parens
  | FunPrec         -- Function args; no parens for tycon apps
  | TyOpPrec        -- Infix operator
  | TyConPrec       -- Tycon args; no parens for atomic
  deriving( Eq, Ord )

pprType :: Type -> Doc
pprType = pprType' TopPrec

pprType' :: TyPrec -> Type -> Doc
pprType' _ (VarTy b)         = pretty b
pprType' p t@(FunTy _ _)     = maybeParens (p < FunPrec) $ hsep $ punctuate "->" (map (pprType' FunPrec) (splitFunTys t))
pprType' p (TyConApp tc tys) = maybeParens (p < TyConPrec) $ pretty tc <+> hsep (map (pprType' TyConPrec) tys)
pprType' p (AppTy a b)       = maybeParens (p < TyConPrec) $ pprType' TyConPrec a <+> pprType' TyConPrec b
pprType' _ t@(ForAllTy _ _)  = let (bs, t') = splitForAlls t
                              in parens $ "forall" <+> hsep (map pretty bs) <> "." <+> pretty t'
pprType' _ LitTy             = "LIT"
pprType' _ CoercionTy        = "Co"

maybeParens :: Bool -> Doc -> Doc
maybeParens True  = parens
maybeParens False = id

instance Pretty Type where
    pretty = pprType

pprExpr :: Expr -> Doc
pprExpr = pprExpr' False

pprExpr' :: Bool -> Expr -> Doc
pprExpr' _parens (EVar v)         = pretty v
pprExpr' _parens (EVarGlobal v)   = pretty v
pprExpr' _parens (ELit l)         = pretty l
pprExpr' parens  (EApp x ys)      = maybeParens parens $ pprExpr' True x <+> sep (map (pprExpr' True) ys)
pprExpr' parens  (ETyLam v x)     = maybeParens parens $ "Λ" <+> pretty v <+> smallRArrow <+> align (pprExpr' False x)
pprExpr' parens  (ELam v x)       = maybeParens parens $ "λ" <+> pretty v <+> smallRArrow <+> align (pprExpr' False x)
pprExpr' parens  (ELet xs y)      = maybeParens parens $ "let" <+> (align $ vcat (map pprBind xs))
                                    <$$> "in" <+> align (pprExpr' False y)
  where pprBind (b, rhs) = pretty b <+> equals <+> align (pprExpr' False rhs)
pprExpr' parens  (ECase x b alts) = maybeParens parens $ "case" <+> pprExpr' False x <+> "of" <+> pretty b <+> "{"
                                    <$$> nest 2 (vsep $ map pprAlt alts)
                                    <> "}"
  where pprAlt (Alt con bndrs rhs) = pretty con <+> hsep (map pretty bndrs) <+> smallRArrow <+> align (pprExpr' False rhs)
pprExpr' parens  (EType t)        = maybeParens parens $ "TYPE:" <+> pprType t
pprExpr' parens  ECoercion        = "CO"

instance Pretty AltCon where
    pretty (AltDataCon t) = text $ T.unpack t
    pretty (AltLit l) = pretty l
    pretty AltDefault = text "DEFAULT"

pprTopBinding :: TopBinding -> Doc
pprTopBinding tb =
    case tb of
      NonRecTopBinding b s rhs -> pprTopBind (b,s,rhs)
      RecTopBinding bs -> "rec" <+> braces (empty <$$> vsep (map pprTopBind bs))
  where
    pprTopBind (b@(Bndr b'),s,rhs) =
        pretty b <+> dcolon <+> pprType (binderType b')
        <$$> comment (pretty s)
        <$$> pretty b <+> equals <+> align (pprExpr rhs)

instance Pretty TopBinding where
    pretty = pprTopBinding

instance Pretty Module where
    pretty m = text "module" <+> pretty (moduleName m) <+> "where"
               <$$> vsep (map pprTopBinding $ moduleBinds m)

comment :: Doc -> Doc
comment x = "{-" <+> x <+> "-}"

dcolon :: Doc
dcolon = "::"

smallRArrow :: Doc
smallRArrow = "→"
