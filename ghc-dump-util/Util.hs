module Util
    ( -- * Convenient IO
      readFile
      -- * Manipulating Types
    , splitFunTys
    , splitForAlls
    ) where

import Ast
import Prelude hiding (readFile)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Serialise.CBOR as CBOR

readFile :: FilePath -> IO Module
readFile fname = CBOR.deserialise <$> BSL.readFile fname

splitFunTys :: Type -> [Type]
splitFunTys = go []
  where
    go acc (FunTy a b) = go (a : acc) b
    go acc t = reverse (t : acc)

splitForAlls :: Type -> ([Binder], Type)
splitForAlls = go []
  where
    go acc (ForAllTy b t) = go (b : acc) t
    go acc t              = (reverse acc, t)
