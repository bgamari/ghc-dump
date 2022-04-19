{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GhcDump.ToHtml (topBindingsToHtml, exprToHtml) where

import Data.List
import Lucid
import GhcDump.Ast
import GhcDump.Util
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Monoid ((<>))
import Prelude

topBindingsToHtml :: [TopBinding] -> Html ()
topBindingsToHtml = foldMap topBindingToHtml

topBindingToHtml :: TopBinding -> Html ()
topBindingToHtml = mapM_ (\(bndr, _, rhs) -> bindingToHtml bndr rhs) . topBindings

divClass :: T.Text -> Html a -> Html a
divClass cls contents = div_ [class_ cls] contents

spanClass :: T.Text -> Html a -> Html a
spanClass cls contents = span_ [class_ cls] contents

keyword :: Html a -> Html a
keyword = divClass "kw"

lambda :: Html ()
lambda = "λ "

rarrow :: Html ()
rarrow = " → "

spaced :: Html () -> Html ()
spaced x = " " <> x <> " "

exprToHtml :: Expr -> Html ()
exprToHtml (EVar v) = bndrToHtml v
exprToHtml (EVarGlobal v) = externalNameToHtml v
exprToHtml (ELit lit) = litToHtml lit
exprToHtml e@(EApp _ _)
  | (x, ys) <- collectArgs e
  = divClass "app" $ do
    exprToHtml x
    " "
    mconcat $ intersperse " " $ map exprToHtml ys
exprToHtml e@(ETyLam _ _)
  | (bndrs, rhs) <- collectTyBinders e
  = divClass "lam" $ do
    lambda
    bndrsToHtml bndrs
    rarrow
    divClass "rhs" $ exprToHtml rhs
exprToHtml e@(ELam _ _)
  | (bndrs, rhs) <- collectBinders e
  = divClass "lam" $ do
    lambda
    bndrsToHtml bndrs
    rarrow
    divClass "rhs" $ exprToHtml rhs
exprToHtml (ELet bs e) = divClass "let" $ do
    keyword "let "
    divClass "binds" $ foldMap (uncurry bindingToHtml) bs
    keyword " in "
    divClass "body" $ exprToHtml e
exprToHtml (ECase scrut b alts) = divClass "case" $ do
    keyword "case "
    divClass "scrut" $ exprToHtml scrut
    keyword " of "
    divClass "alts" $ mapM_ altToHtml alts
exprToHtml (ETick tick e) = divClass "tick" $ do
    keyword "tick "
    exprToHtml e
exprToHtml (EType ty) = divClass "type" $ typeToHtml ty
exprToHtml (ECoercion) = "$co"

bndrToHtml :: Binder -> Html ()
bndrToHtml bndr =
    sigil <> divClass "bndr" (toHtml (binderUniqueName bndr))
  where
    sigil
      | isTyBinder bndr = "@"
      | otherwise = mempty

bndrsToHtml :: [Binder] -> Html ()
bndrsToHtml bndrs =
  divClass "bndrs" $ foldMap (spaced . bndrToHtml) bndrs

typeSigToHtml :: Binder -> Type -> Html ()
typeSigToHtml bndr ty = divClass "sig" $ do
  bndrToHtml bndr
  " :: "
  typeToHtml ty

bindingToHtml :: Binder -> Expr -> Html ()
bindingToHtml bndr rhs = divClass "bind" $ do
  bndrToHtml bndr
  " = "
  divClass "rhs" $ exprToHtml rhs

moduleNameToHtml :: ModuleName -> Html ()
moduleNameToHtml m =
  divClass "mod" $ toHtml $ getModuleName m

externalNameToHtml :: ExternalName -> Html ()
externalNameToHtml (ExternalName mod nam _ _) =
  divClass "ext-name" $ moduleNameToHtml mod <> "." <> toHtml nam
externalNameToHtml (ForeignCall) =
  "$foreign-call"

altToHtml :: Alt -> Html ()
altToHtml Alt{..} = div_ $ do
  case altCon of
    AltDataCon dc -> divClass "datacon" $ toHtml dc
    AltLit lit -> litToHtml lit
    AltDefault -> divClass "kw" "DEFAULT"
  bndrsToHtml altBinders
  rarrow
  divClass "rhs" $ exprToHtml altRHS

typeToHtml :: Type -> Html ()
typeToHtml (VarTy v) = bndrToHtml v
typeToHtml t@(FunTy _ _)
  | ts <- splitFunTys t
  = divClass "funty" $ mconcat $ intersperse rarrow $ map typeToHtml ts
typeToHtml (TyConApp tc tys)
  = divClass "tyconapp" $ tyConToHtml tc <> " " <> mconcat (intersperse " " (map typeToHtml tys))
typeToHtml (AppTy a b)
  = divClass "appty" $ typeToHtml a <> typeToHtml b
typeToHtml t@(ForAllTy _ _)
  | (bndrs, ty) <- splitForAlls t
  = divClass "forallty" $ do
    keyword "forall "
    bndrsToHtml bndrs
    ". "
    typeToHtml t
typeToHtml (LitTy) = "LIT"
typeToHtml (CoercionTy) = "COERCION"

tyConToHtml :: TyCon -> Html ()
tyConToHtml (TyCon name _) = divClass "tycon" $ toHtml name

litToHtml :: Lit -> Html ()
litToHtml (MachChar c) = "'" <> toHtml [c] <> "'"
litToHtml (MachStr s) = "'" <> toHtml (BS.unpack s) <> "'"
litToHtml (MachNullAddr) = "$nullAddr"
litToHtml (MachInt n) = showHtml n <> "#"
litToHtml (MachInt64 n) = showHtml n <> "#"
litToHtml (MachWord n) = showHtml n <> "##"
litToHtml (MachWord64 n) = showHtml n <> "##64"
litToHtml (MachFloat n) = showHtml (realToFrac n :: Double) <> "##"
litToHtml (MachDouble n) = showHtml (realToFrac n :: Double) <> "##"
litToHtml (MachLabel s) = "&" <> toHtml s
litToHtml (LitInteger n) = showHtml n
litToHtml (LitNatural n) = showHtml n

showHtml :: Show a => a -> Html ()
showHtml = toHtml . show
