{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CoreDiff.Pairing where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import CoreDiff.PrettyPrint
import CoreDiff.XAst

data PairingS = PairingS
  { toPair        :: [(XBinding UD, XBinding UD)]
  , unpairedLeft  :: Map (XBinder UD) (XBinding UD)
  , unpairedRight :: Map (XBinder UD) (XBinding UD)
  , paired        :: [XBinding UD]
  }

instance PprWithOpts PairingS where
  pprWithOpts s = do
    pqDoc <- pprPairingQueue $ toList $ toPair s
    ulDoc <- pprUnpaired $ Map.keys $ unpairedLeft s
    urDoc <- pprUnpaired $ Map.keys $ unpairedRight s
    
    return $ vsep
      [ bold $ "To pair:"
      , pqDoc
      , bold $ "Unpaired left:"
      , ulDoc
      , bold $ "Unpaired right:"
      , urDoc
      ]
    where
      pprPairingQueue pq =
        vsep <$> mapM pprPqEntry pq

      pprPqEntry (XBinding binder _, XBinding binder' _) = do
        binderDoc  <- pprWithOpts binder
        binderDoc' <- pprWithOpts binder'
        return $ binderDoc <+> equals <+> binderDoc'

      pprUnpaired u =
        brackets <$> sep <$> punctuate "," <$> mapM pprWithOpts u

pairProg :: XModule -> XModule -> PairingS
pairProg mod mod' = init
  where
    init = PairingS
      trivialPairings
      (toBinderMap mod  `Map.withoutKeys` Set.fromList leftPaired)
      (toBinderMap mod' `Map.withoutKeys` Set.fromList rightPaired)
      []

    trivialPairings =
      triv (xModuleBindings mod) (xModuleBindings mod')
    
    toBinderMap mod = Map.fromList
      [ (binder, binding)
      | binding@(XBinding binder _) <- xModuleBindings mod
      ]

    (leftPaired, rightPaired) = unzip
      [ (binder, binder')
      | (XBinding binder _, XBinding binder' _) <- trivialPairings
      ]

-- | Calculate trivial pairings of exported bindings in two lists of bindings.
triv :: [XBinding UD] -> [XBinding UD] -> [(XBinding UD, XBinding UD)]
triv bindings bindings' =
  [ (binding, nameMap' Map.! xBinderName binder)
  | binding@(XBinding binder _) <- bindings
  , xBinderName binder `Map.member` nameMap'
  , xBinderIsExported binder
  ]
  where
    nameMap' :: Map T.Text (XBinding UD)
    nameMap' = toNameMap bindings'

    toNameMap :: [XBinding UD] -> Map T.Text (XBinding UD)
    toNameMap bindings = Map.fromList
      [ (xBinderName binder, binding)
      | binding@(XBinding binder _) <- bindings
      ]
