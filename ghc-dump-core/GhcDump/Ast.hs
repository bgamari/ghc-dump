{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module GhcDump.Ast where

import GHC.Generics

import Data.Binary.Serialise.CBOR as CBOR
import qualified Data.Text as T

import Unique (mkUnique)

data Unique = Unique !Char !Int
            deriving (Eq, Ord, Generic)
instance Serialise Unique

-- | This is dependent upon GHC
instance Show Unique where
    show (Unique c n) = show $ mkUnique c n

data ExternalName = ExternalName { externalModuleName :: !ModuleName
                                 , externalName :: !T.Text
                                 , externalUnique :: !Unique
                                 }
                  | ForeignCall
                  deriving (Eq, Ord, Generic, Show)
instance Serialise ExternalName

newtype BinderId = BinderId Unique
                 deriving (Eq, Ord, Serialise, Show)

newtype SBinder = SBndr { unSBndr :: Binder' SBinder BinderId }
                deriving (Eq, Ord, Generic, Show)
instance Serialise SBinder

newtype Binder = Bndr { unBndr :: Binder' Binder Binder }
               deriving (Eq, Ord, Generic, Show)
instance Serialise Binder

data Binder' bndr var = Binder { binderName :: !T.Text
                               , binderId   :: !BinderId
                               , binderType :: Type' bndr var
                               }
                      deriving (Eq, Ord, Generic, Show, Functor)
instance (Serialise bndr, Serialise var) => Serialise (Binder' bndr var)

data Lit = SomeLit
         deriving (Generic, Show)
instance Serialise Lit

data TyCon = TyCon !T.Text !Unique
           deriving (Eq, Ord, Generic, Show)
instance Serialise TyCon

type SType = Type' SBinder BinderId
type Type = Type' Binder Binder

data Type' bndr var
    = VarTy var
    | FunTy (Type' bndr var) (Type' bndr var)
    | TyConApp TyCon [Type' bndr var]
    | AppTy (Type' bndr var) (Type' bndr var)
    | ForAllTy bndr (Type' bndr var)
    | LitTy
    | CoercionTy
    deriving (Eq, Ord, Generic, Show, Functor)
instance (Serialise bndr, Serialise var) => Serialise (Type' bndr var)

newtype ModuleName = ModuleName {getModuleName :: T.Text}
                   deriving (Eq, Ord, Serialise, Show)

type Module = Module' Binder Binder
type SModule = Module' SBinder BinderId

data Module' bndr var
    = Module { moduleName        :: ModuleName
             , modulePhase       :: T.Text
             , moduleTopBindings :: [TopBinding' bndr var]
             }
    deriving (Generic, Show)
instance (Serialise bndr, Serialise var) => Serialise (Module' bndr var)

moduleBindings :: Module' bndr var -> [(bndr, CoreStats, Expr' bndr var)]
moduleBindings = concatMap topBindings . moduleTopBindings

-- $binders
--
-- The binder story:
--
-- Things which might contain bound variables (e.g. expressions and types) have
-- a type variable which is instantiated at 'BinderId' in the serialised form or
-- 'Binder' after post-processing.
--
-- Note that bindings sites themselves are always 'Binder's.

type SExpr = Expr' SBinder BinderId
type Expr = Expr' Binder Binder

data Expr' bndr var
    = EVar var
    | EVarGlobal ExternalName
    | ELit Lit
    | EApp (Expr' bndr var) [Expr' bndr var]
    | ETyLam bndr (Expr' bndr var)
    | ELam bndr (Expr' bndr var)
    | ELet [(bndr, Expr' bndr var)] (Expr' bndr var)
    | ECase (Expr' bndr var) bndr [Alt' bndr var]
    | EType (Type' bndr var)
    | ECoercion
    deriving (Generic, Show, Functor)
instance (Serialise bndr, Serialise var) => Serialise (Expr' bndr var)

type SAlt = Alt' SBinder BinderId
type Alt = Alt' Binder Binder

data Alt' bndr var = Alt { altCon     :: !AltCon
                         , altBinders :: [bndr]
                         , altRHS     :: Expr' bndr var
                         }
                  deriving (Generic, Show, Functor)
instance (Serialise bndr, Serialise var) => Serialise (Alt' bndr var)

data AltCon = AltDataCon !T.Text
            | AltLit Lit
            | AltDefault
            deriving (Generic, Show)
instance Serialise AltCon

type STopBinding = TopBinding' SBinder BinderId
type TopBinding = TopBinding' Binder Binder

data TopBinding' bndr var
    = NonRecTopBinding bndr CoreStats (Expr' bndr var)
    | RecTopBinding [(bndr, CoreStats, Expr' bndr var)]
    deriving (Generic, Show, Functor)
instance (Serialise bndr, Serialise var) => Serialise (TopBinding' bndr var)

topBindings :: TopBinding' bndr var -> [(bndr, CoreStats, Expr' bndr var)]
topBindings (NonRecTopBinding a b c) = [(a,b,c)]
topBindings (RecTopBinding bs) = bs

data CoreStats
    = CoreStats { csTerms       :: !Int
                , csTypes       :: !Int
                , csCoercions   :: !Int
                , csValBinds   :: !Int
                , csJoinBinds  :: !Int
                }
    deriving (Generic, Show)
instance Serialise CoreStats

instance Monoid CoreStats where
    mempty = CoreStats 0 0 0 0 0
    CoreStats a b c d e `mappend` CoreStats a' b' c' d' e' =
        CoreStats (a+a') (b+b') (c+c') (d+d') (e+e')
