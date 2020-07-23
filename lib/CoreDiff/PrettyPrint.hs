{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module CoreDiff.PrettyPrint where

import Unsafe.Coerce
import Data.Maybe
import Data.List
import GhcDump.Ast
import qualified Data.Text as T

import CoreDiff.Diff

data PprContext
  = TopLevel
  | Variable
  | ApplicationL
  | ApplicationR
  | LambdaParam
  | LambdaBody
  | Type
  | Let -- bindings in a let expr
  | LetExpr -- the expr in a let expr
  | CaseMatch
  | CaseAlt
  deriving (Eq)

class PrettyPrint t where
  ppr :: PprContext -> t -> String

instance PrettyPrint (Diff BindingC) where
  -- TODO: better distinction between toplevel and let binders
  ppr ctx (BindingC bndr expr) =
    intercalate "\n" $
      [ ppr ctx bndr
      , "* ="
      , indent 2 $ ppr ctx expr
      ]
  -- TODO: BindingHole

instance PrettyPrint (Diff BndrC) where
  -- TODO: distinguish between toplevel, inline, let, etc. binders
  ppr ctx (BndrC bndr) = ifp (ctx == CaseAlt) parens $ ppr ctx bndr
  ppr ctx (BndrHole bndrChg) = ppr ctx bndrChg
  -- TODO: BndrHole

instance PrettyPrint (Diff ExprC) where
  ppr ctx (EVarC bndr) = ppr Variable bndr

  ppr ctx (EVarGlobalC extName) = extName'
    where
      extName' = T.unpack (getModuleName $ externalModuleName extName) ++ "." ++ T.unpack (externalName extName)

  -- TODO: better show here
  ppr _ (ELitC lit) = show lit

  -- TODO: type applications
  ppr ctx (EAppC f x) =
    ifp (ctx == ApplicationR) parens $
      ppr ApplicationL f ++ " " ++ ppr ApplicationR x

  ppr ctx (ETyLamC param body) = pprLambda ctx "@" param body
  ppr ctx (ELamC param body) = pprLambda ctx "" param body

  ppr ctx (ELetC bindings expr) = ifp (ctx `elem` [ApplicationL, ApplicationR]) parens $
    "let\n" ++ indent 2 (intercalate "\n" $ map (ppr Let) bindings) ++ "\nin\n" ++ indent 2 (ppr LetExpr expr)

  -- TODO: display binder if it appears in any alt
  ppr ctx (ECaseC match _bndr alts) = ifp (ctx `elem` [ApplicationL, ApplicationR]) parens $
    "case " ++ ppr CaseMatch match ++ " of\n" ++ indent 2 (intercalate "\n" $ map (ppr CaseAlt) alts)

  -- TODO: make these two prettier
  ppr ctx (ETypeC ty) = prettyPrintType ty

  ppr _ ECoercionC = error "unimplemented"

  ppr ctx (ExprHole exprChg) = ppr ctx exprChg

instance PrettyPrint (Diff AltC) where
  ppr ctx (AltC con bndrs rhs) =
    showAltCon con bndrs ++ " ->\n" ++ indent 2 (ppr CaseAlt rhs)


instance PrettyPrint Binder where
  ppr ctx (Bndr bndr) =
    T.unpack (binderName bndr) ++ "_" ++ showBndrId (binderId bndr) ++ optInfo bndr
    where showBndrId (BinderId bndrId) = show bndrId
          optInfo _ | ctx == Variable = ""
          optInfo (Binder _ _ idi _ ty) = " " ++ prettyPrintIdInfo idi ++ " :: " ++ prettyPrintType ty
          optInfo (TyBinder _ _ kind) = " :: " ++ prettyPrintType kind

-- TODO: this is almost exactly the same as the instance for Diff ExprC
-- TODO: Do we want to be able to edit them seperately or should the somehow be unified?
instance PrettyPrint Expr where
  ppr ctx (EVar bndr) = ppr Variable bndr

  ppr ctx (EVarGlobal extName) = extName'
    where
      extName' = T.unpack (getModuleName $ externalModuleName extName) ++ "." ++ T.unpack (externalName extName)

  -- TODO: better show here
  ppr _ (ELit lit) = show lit

  -- TODO: type applications
  ppr ctx (EApp f x) =
    ifp (ctx == ApplicationR) parens $
      ppr ApplicationL f ++ " " ++ ppr ApplicationR x

  ppr ctx (ETyLam param body) = pprLambda ctx "@" param body
  ppr ctx (ELam param body) = pprLambda ctx "" param body

  ppr ctx (ELet bindings expr) = ifp (ctx `elem` [ApplicationL, ApplicationR]) parens $
    "let\n" ++ indent 2 (intercalate "\n" $ map (ppr Let) bindings) ++ "\nin\n" ++ indent 2 (ppr LetExpr expr)

  -- TODO: display binder if it appears in any alt
  ppr ctx (ECase match _bndr alts) = ifp (ctx `elem` [ApplicationL, ApplicationR]) parens $
    "case " ++ ppr CaseMatch match ++ " of\n" ++ indent 2 (intercalate "\n" $ map (ppr CaseAlt) alts)

  ppr ctx (EType ty) = prettyPrintType ty

  ppr _ ECoercion = error "unimplemented"


instance PrettyPrint (Binder, Expr) where
  -- TODO: better distinction between toplevel and let binders
  ppr ctx (bndr, expr) =
    intercalate "\n" $
      [ ppr ctx bndr
      , "* ="
      , indent 2 $ ppr ctx expr
      ]

instance PrettyPrint Alt where
  ppr ctx (Alt con bndrs rhs) =
    showAltCon con bndrs ++ " ->\n" ++ indent 2 (ppr CaseAlt rhs)

-- TODO: replace by more specific instances, e.g. Change Binder
-- TODO: alternatively, char-by-char classical diff
instance PrettyPrint t => PrettyPrint (Change t) where
  ppr ctx (Change (lhs, rhs)) = showChange (ppr ctx lhs) (ppr ctx rhs)

-- TODO: look at GhcDump/Convert.hs to achieve close-to-original output
prettyPrintIdInfo bInfo = "[" ++ intercalate ", " (catMaybes infos) ++ "]"
  where
    infos =
      [ arity
      , strictness
      , demand
      , occurences
      , unfolding
      ]

    arity =
      if idiArity bInfo == 0
      then Nothing
      else Just $ "Arity=" ++ show (idiArity bInfo)

    strictness =
      toMaybe (idiStrictnessSig bInfo /= "") ("Str=" ++ T.unpack (idiStrictnessSig bInfo))

    demand =
      toMaybe (idiDemandSig bInfo /= "") ("Dmd=" ++ T.unpack (idiDemandSig bInfo))

    occurences = Just $ "Occ=" ++ show' (idiOccInfo bInfo)
      where show' OccManyOccs        = "Many"
            show' OccDead            = "Dead"
            show' OccOneOcc          = "Once"
            show' (OccLoopBreaker _) = "LoopBreaker" -- TODO: show strong/weak loopbreaker (?)

    unfolding =
      toMaybe (idiUnfolding bInfo /= NoUnfolding) ("Unf=" ++ showUnfolding (idiUnfolding bInfo))
      where
        showUnfolding (BootUnfolding) = "BootUnfolding"
        showUnfolding (OtherCon _) = "OtherCon"
        showUnfolding (DFunUnfolding) = "OtherCon"
        showUnfolding unf@(CoreUnfolding {}) = "Unf{" ++ intercalate ", " unfInfos ++ "}"
          where
            unfInfos =
              [ "Value=" ++ show (unfIsValue unf)
              , "ConLike=" ++ show (unfIsConLike unf)
              , "WorkFree=" ++ show (unfIsWorkFree unf)
              , "Guidance=" ++ T.unpack (unfGuidance unf)
              ]

    toMaybe test payload
      | test      = Just payload
      | otherwise = Nothing

prettyPrintType (VarTy bndr) = ppr Variable bndr
prettyPrintType (FunTy lhs rhs) =
  ifp (isFunTy lhs) parens $ (prettyPrintType lhs) ++ " -> " ++ ifp (not $ isFunTy rhs) parens (prettyPrintType rhs)
  where
    isFunTy (FunTy _ _) = True
    isFunTy _           = False
prettyPrintType (TyConApp (TyCon tcName _tcUnique) args) =
  T.unpack tcName ++ optArgs
  where
    optArgs = if length args == 0 then ""
                                  else " " ++ intercalate "\n" (map go args)
    go arg = "(" ++ prettyPrintType arg ++ ")"
prettyPrintType (AppTy _ _) = error "unimplemented"
prettyPrintType (ForAllTy bndr ty) = "forall {" ++ ppr Type bndr ++ "}." ++ prettyPrintType ty
prettyPrintType LitTy = error "unimplemented"
prettyPrintType CoercionTy = error "unimplemented"

-- TODO: use Show term/PrettyPrint term or something
showChange :: String -> String -> String
showChange lhs rhs
  | otherwise = "#( " ++ red lhs ++ "\n / " ++ green rhs ++ "\n )"
  where red = ansiColor 31
        green = ansiColor 32
        ansiColor code str =
          "\ESC[" ++ show code ++ "m" ++ str ++ "\ESC[0m"

-- utilities

indent :: Int -> String -> String
indent n = unlines . map go . lines 
  where go str = replicate n ' ' ++ str

ifp :: Bool -> (a -> a) -> a -> a
ifp p f x
  | p = f x
  | otherwise = x

parens s = "(" ++ s ++ ")"

pprLambda ctx prefix param body =
  -- TODO: multi-argument functions
  ifp (ctx `elem` [ApplicationL, ApplicationR]) parens $
    "\\" ++ prefix ++ ppr LambdaParam param ++ " ->\n" ++ indent 2 (ppr LambdaBody body)

showAltCon :: PrettyPrint a => AltCon -> [a] -> String
showAltCon con bndrs = ifp (length bndrs /= 0) (++ (" " ++ intercalate " " (map (ppr CaseAlt) bndrs))) (showCon con)
  where
    -- TODO: is this alright?
    showCon (AltDataCon conName) = T.unpack conName
    showCon (AltLit lit) = show lit
    showCon (AltDefault) = "DEFAULT"
