{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module GhcDump.Ast where

import GHC.Generics

import Data.Binary.Serialise.CBOR as CBOR
import qualified Data.Text as T

data Unique = Unique !Char !Int
            deriving (Eq, Ord, Generic, Show)
instance Serialise Unique

newtype BinderId = BinderId Unique
                 deriving (Eq, Ord, Serialise, Show)

newtype SBinder = SBndr (Binder' SBinder BinderId)
                deriving (Generic, Show)
instance Serialise SBinder

newtype Binder = Bndr (Binder' Binder Binder)
               deriving (Generic, Show)
instance Serialise Binder

data Binder' bndr var = Binder { binderName :: !T.Text
                               , binderId   :: !BinderId
                               , binderType :: Type' bndr var
                               }
                      deriving (Generic, Show, Functor)
instance (Serialise bndr, Serialise var) => Serialise (Binder' bndr var)

data TyCon = TyCon !T.Text !Unique
           deriving (Generic, Show)
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
    deriving (Generic, Show, Functor)
instance (Serialise bndr, Serialise var) => Serialise (Type' bndr var)

newtype ModuleName = ModuleName T.Text
                   deriving (Serialise, Show)

type Module = Module' Binder Binder
type SModule = Module' SBinder BinderId

data Module' bndr var
    = Module { moduleName :: ModuleName
             , moduleBinds :: [TopBinding' bndr var]
             }
    deriving (Generic, Show)
instance (Serialise bndr, Serialise var) => Serialise (Module' bndr var)

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
    | ELit
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

data Alt' bndr var = Alt T.Text [bndr] (Expr' bndr var)
                  deriving (Generic, Show, Functor)
instance (Serialise bndr, Serialise var) => Serialise (Alt' bndr var)

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
    = CoreStats { cs_terms       :: !Int
                , cs_types       :: !Int
                , cs_coercions   :: !Int
                , cs_val_binds   :: !Int
                , cs_join_binds  :: !Int
                }
    deriving (Generic, Show)
instance Serialise CoreStats
