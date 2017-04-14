{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Ast where

import GHC.Generics

import Data.Binary.Serialise.CBOR as CBOR
import qualified Data.Text as T

data Unique = Unique !Char !Int
            deriving (Generic, Show)
instance Serialise Unique

newtype BinderId = BinderId Unique
                 deriving (Serialise, Show)

data Binder = Binder !T.Text !BinderId
            deriving (Generic, Show)
instance Serialise Binder

data TyCon = TyCon !T.Text !Unique -- Hmm
           deriving (Generic, Show)
instance Serialise TyCon

data TyBinder = NamedTyBinder Binder Type
              | AnonTyBinder Type
              deriving (Generic, Show)
instance Serialise TyBinder

tyBinderKind :: TyBinder -> Type
tyBinderKind (NamedTyBinder _ k) = k
tyBinderKind (AnonTyBinder k)    = k

data Type = VarTy BinderId
          | FunTy Type Type
          | TyConApp TyCon [Type]
          | AppTy Type Type
          | ForAllTy TyBinder Type
          | LitTy
          | CoercionTy
          deriving (Generic, Show)
instance Serialise Type

newtype ModuleName = ModuleName T.Text
                   deriving (Serialise, Show)

data Module = Module { moduleName :: ModuleName
                     , moduleBinds :: [TopBinding]
                     }
            deriving (Generic, Show)
instance Serialise Module

data Expr = EVar BinderId
          | ELit
          | EApp Expr [Expr]
          | ETyLam Binder Expr
          | ELam Binder Expr
          | ELet [(Binder, Expr)] Expr
          | ECase Expr Binder [Alt]
          | EType Type
          | ECoercion
          deriving (Generic, Show)
instance Serialise Expr

data Alt = Alt T.Text [Binder] Expr
         deriving (Generic, Show)
instance Serialise Alt

data TopBinding = NonRecTopBinding Binder CoreStats Expr
                | RecTopBinding [(Binder, CoreStats, Expr)]
                deriving (Generic, Show)
instance Serialise TopBinding

topBindings :: TopBinding -> [(Binder, CoreStats, Expr)]
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
