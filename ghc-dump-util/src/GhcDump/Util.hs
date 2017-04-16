module GhcDump.Util
    ( -- * Convenient IO
      readDump, readDump'
      -- * Manipulating Types
    , splitFunTys
    , splitForAlls
    ) where

import Data.Foldable
import Data.Bifunctor
import Prelude hiding (readFile)

import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Serialise.CBOR as CBOR

import GhcDump.Ast

readDump' :: FilePath -> IO SModule
readDump' fname = CBOR.deserialise <$> BSL.readFile fname

readDump :: FilePath -> IO Module
readDump fname = reconModule <$> readDump' fname

splitFunTys :: Type' bndr var -> [Type' bndr var]
splitFunTys = go []
  where
    go acc (FunTy a b) = go (a : acc) b
    go acc t = reverse (t : acc)

splitForAlls :: Type' bndr var -> ([bndr], Type' bndr var)
splitForAlls = go []
  where
    go acc (ForAllTy b t) = go (b : acc) t
    go acc t              = (reverse acc, t)

newtype BinderMap = BinderMap (HM.HashMap BinderId Binder)

instance Hashable BinderId where
    hashWithSalt salt (BinderId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i

emptyBinderMap :: BinderMap
emptyBinderMap = BinderMap mempty

insertBinder :: Binder -> BinderMap -> BinderMap
insertBinder (Bndr b) (BinderMap m) = BinderMap $ HM.insert (binderId b) (Bndr b) m

insertBinders :: [Binder] -> BinderMap -> BinderMap
insertBinders bs bm = foldl' (flip insertBinder) bm bs

getBinder :: BinderMap -> BinderId -> Binder
getBinder (BinderMap m) bid
  | Just b <- HM.lookup bid m = b
  | otherwise                 = error $ "unknown binder "++ show bid ++ ":\n" ++ show m

-- "recon" == "reconstruct"

reconModule :: SModule -> Module
reconModule m = Module (moduleName m) binds
  where
    binds = map reconTopBinding $ moduleBinds m
    bm = insertBinders (map (\(a,_,_) -> a) $ concatMap topBindings binds) emptyBinderMap

    reconTopBinding :: STopBinding -> TopBinding
    reconTopBinding (NonRecTopBinding b stats rhs) = NonRecTopBinding b' stats (reconExpr bm rhs)
      where b' = reconBinder bm b
    reconTopBinding (RecTopBinding bs) = RecTopBinding bs'
      where bs' = map (\(a,stats,rhs) -> (reconBinder bm a, stats, reconExpr bm rhs)) bs

reconExpr :: BinderMap -> SExpr -> Expr
reconExpr bm (EVar var)       = EVar $ getBinder bm var
reconExpr bm (EVarGlobal n)   = EVarGlobal n
reconExpr bm (ELit l)         = ELit l
reconExpr bm (EApp x ys)      = EApp (reconExpr bm x) (map (reconExpr bm) ys)
reconExpr bm (ETyLam b x)     = let b' = reconBinder bm b
                                    bm' = insertBinder b' bm
                                in ETyLam b' (reconExpr bm' x)
reconExpr bm (ELam b x)       = let b' = reconBinder bm b
                                    bm' = insertBinder b' bm
                                in ELam b' (reconExpr bm' x)
reconExpr bm (ELet bs x)      = let bs' = map (bimap (reconBinder bm) (reconExpr bm')) bs
                                    bm' = insertBinders (map fst bs') bm
                                in ELet bs' (reconExpr bm' x)
reconExpr bm (ECase x b alts) = let b' = reconBinder bm b
                                    bm' = insertBinder b' bm
                                in ECase (reconExpr bm x) b' (map (reconAlt bm') alts)
reconExpr bm (EType t)        = EType (reconType bm t)
reconExpr bm ECoercion        = ECoercion

reconBinder :: BinderMap -> SBinder -> Binder
reconBinder bm (SBndr b) =
    Bndr $ Binder (binderName b) (binderId b) (reconType bm $ binderType b)

reconAlt :: BinderMap -> SAlt -> Alt
reconAlt bm0 (Alt con bs rhs) =
    let doBinders bm acc []       = (bm, reverse acc)
        doBinders bm acc (b:rest) = doBinders bm' (b':acc) rest
          where
            b'  = reconBinder bm b
            bm' = insertBinder b' bm
        (bm', bs') = doBinders bm0 [] bs
    in Alt con bs' (reconExpr bm' rhs)

reconType :: BinderMap -> SType -> Type
reconType bm (VarTy v) = VarTy $ getBinder bm v
reconType bm (FunTy x y) = FunTy (reconType bm x) (reconType bm y)
reconType bm (TyConApp tc tys) = TyConApp tc (map (reconType bm) tys)
reconType bm (AppTy x y) = AppTy (reconType bm x) (reconType bm y)
reconType bm (ForAllTy b x) = let b' = reconBinder bm b
                                  bm' = insertBinder b' bm
                              in ForAllTy b' (reconType bm' x)
reconType bm LitTy = LitTy
reconType bm CoercionTy = CoercionTy
