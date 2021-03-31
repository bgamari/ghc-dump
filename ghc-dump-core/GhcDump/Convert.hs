{-# LANGUAGE CPP #-}
module GhcDump.Convert where

import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Literal (Literal(..))
import qualified GHC.Types.Literal as Literal
import GHC.Types.Var (Var(..))
import qualified GHC.Types.Var as Var
import GHC.Types.Id (isFCallId)
import GHC.Unit.Module as Module (moduleName)
import GHC.Unit.Module.Name as Module (ModuleName, moduleNameFS)
import GHC.Types.Name (getOccName, occNameFS, OccName, getName, nameModule_maybe)
import qualified GHC.Types.Id.Info as IdInfo
import qualified GHC.Types.Basic as OccInfo (OccInfo(..), isStrongLoopBreaker)
import qualified GHC.Core.Stats as CoreStats
import qualified GHC.Core as CoreSyn
import GHC.Core (Expr(..), CoreExpr, Bind(..), CoreAlt, CoreBind, AltCon(..))
import GHC.Driver.Types (ModGuts(..))
import GHC.Data.FastString (FastString)
import qualified GHC.Data.FastString as FastString
import qualified GHC.Core.TyCo.Rep as Type
import GHC.Core.TyCon as TyCon (TyCon, tyConUnique)
import GHC.Utils.Outputable (ppr, showSDoc, SDoc)
import GHC.Types.Unique as Unique (Unique, getUnique, unpkUnique)
import GHC.Driver.Session (unsafeGlobalDynFlags)
import GHC.Driver.Session (unsafeGlobalDynFlags)
import qualified GHC.Types.SrcLoc as SrcLoc

#else

import Literal (Literal(..))
#if MIN_VERSION_ghc(8,6,0)
import qualified Literal
#endif
import Var (Var)
import qualified Var
import Id (isFCallId)
import Module (ModuleName, moduleNameFS, moduleName)
import Unique (Unique, getUnique, unpkUnique)
import Name (getOccName, occNameFS, OccName, getName, nameModule_maybe)
import qualified SrcLoc
import qualified IdInfo
import qualified BasicTypes as OccInfo (OccInfo(..), isStrongLoopBreaker)
#if MIN_VERSION_ghc(8,0,0)
import qualified CoreStats
#else
import qualified CoreUtils as CoreStats
#endif
import qualified CoreSyn
import CoreSyn (Expr(..), CoreExpr, Bind(..), CoreAlt, CoreBind, AltCon(..), Tickish(..))
import HscTypes (ModGuts(..))
import FastString (FastString)
import qualified FastString
#if MIN_VERSION_ghc(8,2,0)
import TyCoRep as Type (Type(..))
#elif MIN_VERSION_ghc(8,0,0)
import TyCoRep as Type (Type(..), TyBinder(..))
#else
import TypeRep as Type (Type(..))
#endif
#if !(MIN_VERSION_ghc(8,2,0))
import Type (splitFunTy_maybe)
#endif
import TyCon (TyCon, tyConUnique)

import Outputable (ppr, showSDoc, SDoc)
import DynFlags (unsafeGlobalDynFlags)
#endif

import GhcDump.Ast as Ast

cvtSDoc :: SDoc -> T.Text
cvtSDoc = T.pack . showSDoc unsafeGlobalDynFlags

fastStringToText :: FastString -> T.Text
fastStringToText = TE.decodeUtf8
#if MIN_VERSION_ghc(8,10,0)
  . FastString.bytesFS
#else
  . FastString.fastStringToByteString
#endif

occNameToText :: OccName -> T.Text
occNameToText = fastStringToText . occNameFS

cvtUnique :: Unique.Unique -> Ast.Unique
cvtUnique u =
    let (a,b) = unpkUnique u
    in Ast.Unique a b

cvtVar :: Var -> BinderId
cvtVar = BinderId . cvtUnique . Var.varUnique

cvtBinder :: Var -> SBinder
cvtBinder v
  | Var.isId v =
    SBndr $ Binder { binderName   = occNameToText $ getOccName v
                   , binderId     = cvtVar v
                   , binderIdInfo = cvtIdInfo $ Var.idInfo v
                   , binderIdDetails = cvtIdDetails $ Var.idDetails v
                   , binderType   = cvtType $ Var.varType v
                   }
  | otherwise =
    SBndr $ TyBinder { binderName   = occNameToText $ getOccName v
                     , binderId     = cvtVar v
                     , binderKind   = cvtType $ Var.varType v
                     }

cvtIdInfo :: IdInfo.IdInfo -> Ast.IdInfo SBinder BinderId
cvtIdInfo i =
    IdInfo { idiArity         = IdInfo.arityInfo i
           , idiIsOneShot     = IdInfo.oneShotInfo i == IdInfo.OneShotLam
           , idiUnfolding     = cvtUnfolding $ IdInfo.unfoldingInfo i
           , idiInlinePragma  = cvtSDoc $ ppr $ IdInfo.inlinePragInfo i
           , idiOccInfo       = case IdInfo.occInfo i of
#if MIN_VERSION_ghc(8,2,0)
                                  OccInfo.ManyOccs{} -> OccManyOccs
#else
                                  OccInfo.NoOccInfo  -> OccManyOccs
#endif
                                  OccInfo.IAmDead    -> OccDead
                                  OccInfo.OneOcc{}   -> OccOneOcc
                                  oi@OccInfo.IAmALoopBreaker{} -> OccLoopBreaker (OccInfo.isStrongLoopBreaker oi)
           , idiStrictnessSig = cvtSDoc $ ppr $ IdInfo.strictnessInfo i
           , idiDemandSig     = cvtSDoc $ ppr $ IdInfo.demandInfo i
           , idiCallArity     = IdInfo.callArityInfo i
           }

cvtUnfolding :: CoreSyn.Unfolding -> Ast.Unfolding SBinder BinderId
cvtUnfolding CoreSyn.NoUnfolding = Ast.NoUnfolding
#if MIN_VERSION_ghc(8,2,0)
cvtUnfolding CoreSyn.BootUnfolding = Ast.BootUnfolding
#endif
cvtUnfolding (CoreSyn.OtherCon cons) = Ast.OtherCon (map cvtAltCon cons)
cvtUnfolding (CoreSyn.DFunUnfolding{}) = Ast.DFunUnfolding
cvtUnfolding u@(CoreSyn.CoreUnfolding{}) =
    Ast.CoreUnfolding { unfTemplate   = cvtExpr $ CoreSyn.uf_tmpl u
                      , unfIsValue    = CoreSyn.uf_is_value u
                      , unfIsConLike  = CoreSyn.uf_is_conlike u
                      , unfIsWorkFree = CoreSyn.uf_is_work_free u
                      , unfGuidance   = cvtSDoc $ ppr $ CoreSyn.uf_guidance u
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
exprStats _ = CoreStats.CS 0 0 0
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
        -- foreign calls are local but have no binding site.
        -- TODO: use hasNoBinding here.
      | isFCallId x   -> EVarGlobal ForeignCall
      | Just m <- nameModule_maybe $ getName x
                      -> EVarGlobal $ ExternalName (cvtModuleName $ Module.moduleName m)
                                                   (occNameToText $ getOccName x)
                                                   (cvtUnique $ getUnique x)
      | otherwise     -> EVar (cvtVar x)
    Lit l             -> ELit (cvtLit l)
    App x y           -> EApp (cvtExpr x) (cvtExpr y)
    Lam x e
      | Var.isTyVar x -> ETyLam (cvtBinder x) (cvtExpr e)
      | otherwise     -> ELam (cvtBinder x) (cvtExpr e)
    Let (NonRec b e) body -> ELet [(cvtBinder b, cvtExpr e)] (cvtExpr body)
    Let (Rec bs) body -> ELet (map (bimap cvtBinder cvtExpr) bs) (cvtExpr body)
    Case e x _ as     -> ECase (cvtExpr e) (cvtBinder x) (map cvtAlt as)
    Cast x _          -> cvtExpr x
    Tick tick e
      | CoreSyn.SourceNote sspan _name <- tick
                      -> ETick (Ast.SourceNote $ cvtRealSrcSpan sspan) (cvtExpr e)
      | otherwise     -> cvtExpr e
    Type t            -> EType $ cvtType t
    Coercion _        -> ECoercion

cvtRealSrcSpan :: SrcLoc.RealSrcSpan -> SrcSpan
cvtRealSrcSpan span =
  Ast.SrcSpan { spanFile  = T.pack $ show $ SrcLoc.srcSpanFile span
              , spanStart = LineCol (SrcLoc.srcSpanStartLine span) (SrcLoc.srcSpanStartCol span)
              , spanEnd   = LineCol (SrcLoc.srcSpanEndLine span) (SrcLoc.srcSpanEndCol span)
              }

cvtAlt :: CoreAlt -> Ast.SAlt
cvtAlt (con, bs, e) = Alt (cvtAltCon con) (map cvtBinder bs) (cvtExpr e)

cvtAltCon :: CoreSyn.AltCon -> Ast.AltCon
cvtAltCon (DataAlt altcon) = Ast.AltDataCon $ occNameToText $ getOccName altcon
cvtAltCon (LitAlt l)       = Ast.AltLit $ cvtLit l
cvtAltCon DEFAULT          = Ast.AltDefault

cvtLit :: Literal -> Ast.Lit
cvtLit l =
    case l of
#if MIN_VERSION_ghc(8,8,0)
      Literal.LitChar x -> Ast.MachChar x
      Literal.LitString x -> Ast.MachStr x
      Literal.LitNullAddr -> Ast.MachNullAddr
      Literal.LitFloat x -> Ast.MachFloat x
      Literal.LitDouble x -> Ast.MachDouble x
      Literal.LitLabel x _ _ -> Ast.MachLabel $ fastStringToText  x
      Literal.LitRubbish -> Ast.LitRubbish
#else
      Literal.MachChar x -> Ast.MachChar x
      Literal.MachStr x -> Ast.MachStr x
      Literal.MachNullAddr -> Ast.MachNullAddr
      Literal.MachFloat x -> Ast.MachFloat x
      Literal.MachDouble x -> Ast.MachDouble x
      Literal.MachLabel x _ _ -> Ast.MachLabel $ fastStringToText  x
#endif
#if MIN_VERSION_ghc(8,6,0)
#if MIN_VERSION_ghc(9,0,0)
      Literal.LitNumber numty n ->
#else
      Literal.LitNumber numty n _ ->
#endif
        case numty of
          Literal.LitNumInt -> Ast.MachInt n
          Literal.LitNumInt64 -> Ast.MachInt64 n
          Literal.LitNumWord -> Ast.MachWord n
          Literal.LitNumWord64 -> Ast.MachWord64 n
          Literal.LitNumInteger -> Ast.LitInteger n
          Literal.LitNumNatural -> Ast.LitNatural n
#else
      Literal.MachInt x -> Ast.MachInt x
      Literal.MachInt64 x -> Ast.MachInt64 x
      Literal.MachWord x -> Ast.MachWord x
      Literal.MachWord64 x -> Ast.MachWord64 x
      Literal.LitInteger x _ -> Ast.LitInteger x
#endif

cvtModule :: String -> ModGuts -> Ast.SModule
cvtModule phase guts =
    Ast.Module name (T.pack phase) (map cvtTopBind $ mg_binds guts)
  where name = cvtModuleName $ Module.moduleName $ mg_module guts

cvtModuleName :: Module.ModuleName -> Ast.ModuleName
cvtModuleName = Ast.ModuleName . fastStringToText . moduleNameFS

cvtType :: Type.Type -> Ast.SType
#if MIN_VERSION_ghc(9,0,0)
cvtType (Type.FunTy _flag _ a b) = Ast.FunTy (cvtType a) (cvtType b)
#elif MIN_VERSION_ghc(8,10,0)
cvtType (Type.FunTy _flag a b) = Ast.FunTy (cvtType a) (cvtType b)
#elif MIN_VERSION_ghc(8,2,0)
cvtType (Type.FunTy a b) = Ast.FunTy (cvtType a) (cvtType b)
#else
cvtType t
  | Just (a,b) <- splitFunTy_maybe t = Ast.FunTy (cvtType a) (cvtType b)
#endif
cvtType (Type.TyVarTy v)       = Ast.VarTy (cvtVar v)
cvtType (Type.AppTy a b)       = Ast.AppTy (cvtType a) (cvtType b)
cvtType (Type.TyConApp tc tys) = Ast.TyConApp (cvtTyCon tc) (map cvtType tys)
#if MIN_VERSION_ghc(8,8,0)
cvtType (Type.ForAllTy (Var.Bndr b _) t) = Ast.ForAllTy (cvtBinder b) (cvtType t)
#elif MIN_VERSION_ghc(8,2,0)
cvtType (Type.ForAllTy (Var.TvBndr b _) t) = Ast.ForAllTy (cvtBinder b) (cvtType t)
#elif MIN_VERSION_ghc(8,0,0)
cvtType (Type.ForAllTy (Named b _) t) = Ast.ForAllTy (cvtBinder b) (cvtType t)
cvtType (Type.ForAllTy (Anon _) t)    = cvtType t
#else
cvtType (Type.ForAllTy b t)    = Ast.ForAllTy (cvtBinder b) (cvtType t)
#endif
cvtType (Type.LitTy _)         = Ast.LitTy
#if MIN_VERSION_ghc(8,0,0)
cvtType (Type.CastTy t _)      = cvtType t
cvtType (Type.CoercionTy _)    = Ast.CoercionTy
#endif

cvtTyCon :: TyCon.TyCon -> Ast.TyCon
cvtTyCon tc = TyCon (occNameToText $ getOccName tc) (cvtUnique $ tyConUnique tc)
