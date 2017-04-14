{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Ast where

import GHC.Generics

import Data.Binary.Serialise.CBOR as CBOR
import qualified Data.Text as T

import CoreSyn (AltCon)

data Unique = Unique !Char !Int
            deriving (Generic)
instance Serialise Unique

newtype BinderId = BinderId Unique
                 deriving (Serialise)

data Binder = Binder !T.Text !BinderId
            deriving (Generic)
instance Serialise Binder

data TyCon = TyCon !T.Text !BinderId -- Hmm
           deriving (Generic)
instance Serialise TyCon

data TyBinder = NamedTyBinder Binder Type
              | AnonTyBinder Type
              deriving (Generic)
instance Serialise TyBinder

data Type = VarTy BinderId
          | FunTy Type Type
          | TyConApp TyCon [Type]
          | AppTy Type Type
          | ForAllTy TyBinder Type
          | LitTy
          | CoercionTy
          deriving (Generic)
instance Serialise Type

newtype ModuleName = ModuleName T.Text
                   deriving (Serialise)

data Module = Module ModuleName [TopBinding]
            deriving (Generic)
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
          deriving (Generic)
instance Serialise Expr

data Alt = Alt T.Text [Binder] Expr
         deriving (Generic)
instance Serialise Alt

data TopBinding = NonRecTopBinding Binder CoreStats Expr
                | RecTopBinding [(Binder, CoreStats, Expr)]
                deriving (Generic)
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
    deriving (Generic)
instance Serialise CoreStats

