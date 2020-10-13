{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module CoreDiff.PrettyPrint where

import Control.Monad.Trans.Reader
import Data.Maybe
import qualified Data.Text as T
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
        metaDoc <- pprWithOpts $ xBinderMeta binder
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
        metaDoc <- pprWithOpts $ xBinderMeta binder
        typeDoc <- pprWithOpts $ xBinderTypeOrKind binder
        exprDoc <- pprWithOpts expr

        return $ hang' (hsep $ catMaybes [ Just binderNameDoc, toMaybe (pprOptsDisplayMeta opts) metaDoc, Just dcolon, Just typeDoc, Just equals]) 2 exprDoc


instance PprWithOpts (XType a) where
  pprWithOpts _ = return $ text "tbd"

instance PprWithOpts (XExpr a) where
  pprWithOpts _ = return $ text "tbd"

instance PprWithOpts XBinderMeta where
  pprWithOpts _ = return $ text "tbd"


-- Terminals

-- | Takes care of printing binders with or without uniques.
pprBinderName binder = do
  opts <- ask
  if pprOptsDisplayUniques opts then
    return $ text' $ xBinderName binder <> "_" <> T.pack (show $ xBinderId binder)
  else
    return $ text' $ xBinderName binder

-- Helper functions

dcolon = text "::"

-- | Copied from @GhcDump.Pretty@.
hang' left indent right = hang indent $ sep [ left, right ]

toMaybe True  x = Just x
toMaybe False _ = Nothing
