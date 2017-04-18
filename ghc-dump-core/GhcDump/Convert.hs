{-# LANGUAGE CPP #-}
module GhcDump.Convert where

import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Literal (Literal(..))
import Var (Var, varUnique, varType, isTyVar)
import Id (isFCallId)
import qualified Var
import Module (ModuleName, moduleNameFS, moduleName)
import Unique (Unique, getUnique, unpkUnique)
import Name (getOccName, occNameFS, OccName, getName, nameModule_maybe)
import qualified IdInfo
import qualified BasicTypes as OccInfo (OccInfo(..), isStrongLoopBreaker)
#if MIN_VERSION_ghc(8,0,0)
import qualified CoreStats
#else
import qualified CoreUtils as CoreStats
#endif
import CoreSyn (Expr(..), CoreExpr, Bind(..), CoreAlt, CoreBind, collectArgs, AltCon(..))
import HscTypes (ModGuts(..))
import FastString (FastString, fastStringToByteString)
#if MIN_VERSION_ghc(8,0,0)
import TyCoRep as Type (Type(..), TyBinder(..))
#else
import TypeRep as Type (Type(..))
#endif
import Type (splitFunTy_maybe)
import TyCon (TyCon, tyConUnique)

import Outputable (ppr, showSDoc, SDoc)
import DynFlags (unsafeGlobalDynFlags)

import GhcDump.Ast as Ast

cvtSDoc :: SDoc -> T.Text
cvtSDoc = T.pack . showSDoc unsafeGlobalDynFlags

fastStringToText :: FastString -> T.Text
fastStringToText = TE.decodeUtf8 . fastStringToByteString

occNameToText :: OccName -> T.Text
occNameToText = fastStringToText . occNameFS

cvtUnique :: Unique.Unique -> Ast.Unique
cvtUnique u =
    let (a,b) = unpkUnique u
    in Ast.Unique a b

cvtVar :: Var -> BinderId
cvtVar = BinderId . cvtUnique . varUnique

cvtBinder :: Var -> SBinder
cvtBinder v =
    SBndr $ Binder { binderName   = occNameToText $ getOccName v
                   , binderId     = cvtVar v
                   , binderIdInfo = cvtIdInfo $ Var.idInfo v
                   , binderIdDetails = cvtIdDetails $ Var.idDetails v
                   , binderType   = cvtType $ Var.varType v
                   }

cvtIdInfo :: IdInfo.IdInfo -> Ast.IdInfo
cvtIdInfo i =
    IdInfo { idiArity         = IdInfo.arityInfo i
           , idiIsOneShot     = IdInfo.oneShotInfo i == IdInfo.OneShotLam
           , idiOccInfo       = case IdInfo.occInfo i of
#if MIN_VERSION_ghc(8,2,0)
                                  OccInfo.ManyOccs{} -> OccManyOccs
#endif
                                  OccInfo.IAmDead    -> OccDead
                                  OccInfo.OneOcc{}   -> OccOneOcc
                                  oi@OccInfo.IAmALoopBreaker{} -> OccLoopBreaker (OccInfo.isStrongLoopBreaker oi)
           , idiStrictnessSig = cvtSDoc $ ppr $ IdInfo.strictnessInfo i
           , idiDemandSig     = cvtSDoc $ ppr $ IdInfo.demandInfo i
           , idiCallArity     = IdInfo.callArityInfo i
           }

cvtIdDetails :: IdInfo.IdDetails -> Ast.IdDetails
cvtIdDetails d =
    case d of
      IdInfo.VanillaId -> Ast.VanillaId
      IdInfo.RecSelId{} -> Ast.RecSelId
      IdInfo.DataConWorkId{} -> Ast.DataConWorkId
      IdInfo.DataConWrapId{} -> Ast.DataConWrapId
      IdInfo.ClassOpId{} -> Ast.ClassOpId
      IdInfo.PrimOpId{} -> Ast.PrimOpId
      IdInfo.FCallId{} -> error "This shouldn't happen"
      IdInfo.TickBoxOpId{} -> Ast.TickBoxOpId
      IdInfo.DFunId{} -> Ast.DFunId
#if MIN_VERSION_ghc(8,0,0)
      IdInfo.CoVarId{} -> Ast.CoVarId
#endif
#if MIN_VERSION_ghc(8,2,0)
      IdInfo.JoinId n -> Ast.JoinId n
#endif

cvtCoreStats :: CoreStats.CoreStats -> Ast.CoreStats
cvtCoreStats stats =
    Ast.CoreStats
      { csTerms     = CoreStats.cs_tm stats
      , csTypes     = CoreStats.cs_ty stats
      , csCoercions = CoreStats.cs_co stats
#if MIN_VERSION_ghc(8,2,0)
      , csValBinds  = CoreStats.cs_vb stats
      , csJoinBinds = CoreStats.cs_jb stats
#else
      , csValBinds  = 0
      , csJoinBinds = 0
#endif
      }

exprStats :: CoreExpr -> CoreStats.CoreStats
#if MIN_VERSION_ghc(8,0,0)
exprStats = CoreStats.exprStats
#else
-- exprStats wasn't exported in 7.10
exprStats expr = CoreStats.CS 0 0 0
#endif

cvtTopBind :: CoreBind -> STopBinding
cvtTopBind (NonRec b e) =
    NonRecTopBinding (cvtBinder b) (cvtCoreStats $ exprStats e) (cvtExpr e)
cvtTopBind (Rec bs) =
    RecTopBinding $ map to bs
  where to (b, e) = (cvtBinder b, cvtCoreStats $ exprStats e, cvtExpr e)

cvtExpr :: CoreExpr -> Ast.SExpr
cvtExpr expr =
  case expr of
    Var x
        -- foreign calls are local but have no binding site
      | isFCallId x   -> EVarGlobal ForeignCall
      | Just mod <- nameModule_maybe $ getName x
                      -> EVarGlobal $ ExternalName (cvtModuleName $ Module.moduleName mod)
                                                   (occNameToText $ getOccName x)
                                                   (cvtUnique $ getUnique x)
      | otherwise     -> EVar (cvtVar x)
    Lit l             -> ELit (cvtLit l)
    App {}            -> let (x, ys) = collectArgs expr
                         in EApp (cvtExpr x) (map cvtExpr ys)
    Lam x e
      | isTyVar x     -> ETyLam (cvtBinder x) (cvtExpr e)
      | otherwise     -> ELam (cvtBinder x) (cvtExpr e)
    Let (NonRec b e) body -> ELet [(cvtBinder b, cvtExpr e)] (cvtExpr body)
    Let (Rec bs) body -> ELet (map (bimap cvtBinder cvtExpr) bs) (cvtExpr body)
    Case e x _ as     -> ECase (cvtExpr e) (cvtBinder x) (map cvtAlt as)
    Cast x _          -> cvtExpr x
    Tick _ e          -> cvtExpr e
    Type t            -> EType $ cvtType t
    Coercion _        -> ECoercion

cvtAlt :: CoreAlt -> Ast.SAlt
cvtAlt (con, bs, e) = Alt con' (map cvtBinder bs) (cvtExpr e)
  where
    con' = case con of
             DataAlt con -> Ast.AltDataCon $ occNameToText $ getOccName con
             LitAlt l    -> Ast.AltLit $ cvtLit l
             DEFAULT     -> Ast.AltDefault

cvtLit :: Literal -> Ast.Lit
cvtLit l = SomeLit -- TODO

cvtModule :: String -> ModGuts -> Ast.SModule
cvtModule phase guts =
    Ast.Module name (T.pack phase) (map cvtTopBind $ mg_binds guts)
  where name = cvtModuleName $ Module.moduleName $ mg_module guts

cvtModuleName :: Module.ModuleName -> Ast.ModuleName
cvtModuleName = Ast.ModuleName . fastStringToText . moduleNameFS

cvtType :: Type.Type -> Ast.SType
cvtType t
  | Just (a,b) <- splitFunTy_maybe t = Ast.FunTy (cvtType a) (cvtType b)
cvtType (Type.TyVarTy v)       = Ast.VarTy (cvtVar v)
cvtType (Type.AppTy a b)       = Ast.AppTy (cvtType a) (cvtType b)
cvtType (Type.TyConApp tc tys) = Ast.TyConApp (cvtTyCon tc) (map cvtType tys)
#if MIN_VERSION_ghc(8,2,0)
cvtType (Type.ForAllTy (Var.TvBndr b _) t) = Ast.ForAllTy (cvtBinder b) (cvtType t)
#elif MIN_VERSION_ghc(8,0,0)
cvtType (Type.ForAllTy (Named b _) t) = Ast.ForAllTy (cvtBinder b) (cvtType t)
cvtType (Type.ForAllTy (Anon _) t)    = cvtType t
#else
cvtType (Type.ForAllTy b t)    = Ast.ForAllTy (cvtBinder b) (cvtType t)
#endif
cvtType (Type.LitTy l)         = Ast.LitTy
#if MIN_VERSION_ghc(8,0,0)
cvtType (Type.CastTy t _)      = cvtType t
cvtType (Type.CoercionTy _)    = Ast.CoercionTy
#endif

cvtTyCon :: TyCon.TyCon -> Ast.TyCon
cvtTyCon tc = TyCon (occNameToText $ getOccName tc) (cvtUnique $ tyConUnique tc)
