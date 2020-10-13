{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CoreDiff.Pairing where

import Data.Foldable (toList)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import GhcDump.Ast (AltCon)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import CoreDiff.PrettyPrint
import CoreDiff.XAst

data PairingS = PairingS
  { toPair        :: Seq (XBinding UD, XBinding UD)
  , unpairedLeft  :: Map (XBinder UD) (XBinding UD)
  , unpairedRight :: Map (XBinder UD) (XBinding UD)
  , paired        :: [(XBinding UD, XBinding UD)]
  }

instance PprWithOpts PairingS where
  pprWithOpts (PairingS pq unpairedL unpairedR paired) = do
    pqDoc <- pprPairingQueue $ toList pq
    ulDoc <- pprUnpaired $ Map.keys unpairedL
    urDoc <- pprUnpaired $ Map.keys unpairedR
    pDoc  <- pprPairingQueue paired
    
    return $ vsep
      [ bold $ "To pair:"
      , pqDoc
      , bold $ "Unpaired left:"
      , ulDoc
      , bold $ "Unpaired right:"
      , urDoc
      , bold $ "Paired:"
      , pDoc
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
pairProg mod mod' = iter $ PairingS
  (Seq.fromList trivialPairings)
  (toBinderMap (xModuleBindings mod)  `Map.withoutKeys` Set.fromList leftPaired)
  (toBinderMap (xModuleBindings mod') `Map.withoutKeys` Set.fromList rightPaired)
  []
  where
    trivialPairings =
      triv (xModuleBindings mod) (xModuleBindings mod')
    (leftPaired, rightPaired) = deconPairings trivialPairings

pairLet :: (XExpr UD, XExpr UD) -> ([XBinding UD], [XBinding UD]) -> PairingS
pairLet (expr, expr') (bindings, bindings') = iter $ PairingS
  (Seq.fromList disagreeingBindings)
  (bindingsMap  `Map.withoutKeys` Set.fromList leftPaired)
  (bindingsMap' `Map.withoutKeys` Set.fromList rightPaired)
  []
  where
    disagreeing = disExpr expr expr'
    disagreeingBindings =
      [ (bindingsMap Map.! binder, bindingsMap' Map.! binder')
      | (binder, binder') <- disagreeing
      , binder  `Map.member` bindingsMap
      , binder' `Map.member` bindingsMap'
      ]
    (leftPaired, rightPaired) = deconPairings disagreeingBindings

    bindingsMap  = toBinderMap bindings
    bindingsMap' = toBinderMap bindings'

deconPairings pq = unzip
  [ (binder, binder')
  | (XBinding binder _, XBinding binder' _) <- pq
  ]

toBinderMap bindings = Map.fromList
  [ (binder, binding)
  | binding@(XBinding binder _) <- bindings
  ]

-- | Calculate trivial pairings of exported bindings in two lists of bindings.
triv :: [XBinding UD] -> [XBinding UD] -> [(XBinding UD, XBinding UD)]
triv bindings bindings' =
  rmPair ++ triv' bindingsL bindingsR
  where
    (rmPair, bindingsL, bindingsR) = pairRootMainIfExists bindings bindings'

    -- Modules with a main function also always have an additional *exported* main function that ALWAYS has the unique 01D (= tag:0, id:101, since it's base62).
    -- The line where this binding's name is created is a whopping 8 years old!
    -- https://gitlab.haskell.org/ghc/ghc/-/blob/a1f34d37b47826e86343e368a5c00f1a4b1f2bce/compiler/GHC/Tc/Module.hs#L1794
    -- https://gitlab.haskell.org/ghc/ghc/-/blob/a1f34d37b47826e86343e368a5c00f1a4b1f2bce/compiler/GHC/Builtin/Names.hs#L2245
    -- There is even a note about this here: https://gitlab.haskell.org/ghc/ghc/-/blob/a1f34d37b47826e86343e368a5c00f1a4b1f2bce/compiler/GHC/Tc/Module.hs#L1893
    -- This confuses the "trivial pairings" algorithm, which assumes that exported binders have unambiguous names.
    -- This code is very dirty and will probably be exchanged for a more general solution in the future.
    pairRootMainIfExists bindings bindings' =
      case (find isRootMain bindings, find isRootMain bindings') of
        (Just rm, Just rm') ->
          ([(rm, rm')], filter (not . isRootMain) bindings, filter (not . isRootMain) bindings')
        _ ->
          ([], bindings, bindings')

    -- comparing the data types directly would mean we'd have to import Unique.mkUnique from ghc.
    -- We'd rather only transitively depend on it through ghc-dump.
    isRootMain (XBinding binder _) = show (xBinderId binder) == "01D"

    triv' bindings bindings' =
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

-- | Repeatedly apply @step@ until @toPair@ is empty.
iter :: PairingS -> PairingS
iter s
  | null $ toPair s = s
  | otherwise       = iter $ step s

step :: PairingS -> PairingS
step (PairingS pq unpairedL unpairedR paired) = PairingS
  (pq' >< Seq.fromList newPairings)
  (unpairedL `Map.withoutKeys` Set.fromList newPairingBindersL)
  (unpairedR `Map.withoutKeys` Set.fromList newPairingBindersR)
  ((binding, binding') : paired)
  where
    (binding@(XBinding _ expr), binding'@(XBinding _ expr')) :<| pq' = pq
    disagreeing = disExpr expr expr'
    newPairings = catMaybes $ map go disagreeing
      where
        go (binder, binder') =
          case (unpairedL Map.!? binder, unpairedR Map.!? binder') of
            (Just b, Just b') -> Just (b, b')
            _                 -> Nothing
    (newPairingBindersL, newPairingBindersR) = unzip
      [ (binder, binder')
      | (XBinding binder _, XBinding binder' _) <- newPairings
      ]

disExpr :: XExpr UD -> XExpr UD -> [(XBinder UD, XBinder UD)]
disExpr expr expr' =
  [ (binder, binder')
  | (binder, binder') <- allDisagreeing
  , not $ binder `Set.member` boundInExpr
  , not $ binder `Set.member` boundInExpr'
  ]
  where
    (allDisagreeing, boundInExpr, boundInExpr') = disExpr' expr expr'

    disExpr' (XVar binder) (XVar binder') = ([(binder, binder')], Set.empty, Set.empty)
    disExpr' (XApp f x) (XApp f' x') =
      (d ++ d', boundInF `Set.union` boundInX, boundInF' `Set.union` boundInX')
      where
        (d , boundInF, boundInF') = disExpr' f f'
        (d', boundInX, boundInX') = disExpr' x x'
    disExpr' (XTyLam p b) (XTyLam p' b') =
      (d, Set.insert p boundInB, Set.insert p' boundInB')
      where
        (d, boundInB, boundInB') = disExpr' b b'
    disExpr' (XLam p b) (XLam p' b') =
      (d, Set.insert p boundInB, Set.insert p' boundInB')
      where
        (d, boundInB, boundInB') = disExpr' b b'
    disExpr' (XLet bindings expr) (XLet bindings' expr') =
      ( concat disagreeings ++ disExpr expr expr'
      , Set.fromList [ binder  | XBinding binder  _ <- bindings  ]
      , Set.fromList [ binder' | XBinding binder' _ <- bindings' ]
      )
      where
        s@(PairingS _ _ _ paired) =
          pairLet (expr, expr') (bindings, bindings')
        disagreeings =
          [ disExpr expr expr'
          | (XBinding _ expr, XBinding _ expr') <- paired
          ]
        
    disExpr' (XCase scrut binder alts) (XCase scrut' binder' alts') =
      -- ( disExpr scrut scrut' ++ go alts alts'
      ( scrutDis ++ disAlts alts alts'
      , Set.singleton binder
      , Set.singleton binder'
      )
      where
        scrutDis = disExpr scrut scrut'
    -- We don't handle types yet.
    disExpr' _             _              = ([], Set.empty, Set.empty)

disAlts alts alts' = concat
  [ altDis (altConMap Map.! altCon) (altConMap' Map.! altCon)
  | altCon <- toList $ Set.intersection (Map.keysSet altConMap) (Map.keysSet altConMap')
  ]
  where
    altConMap  = toAltConMap alts
    altConMap' = toAltConMap alts'

    toAltConMap :: [XAlt UD] -> Map AltCon (XAlt UD)
    toAltConMap alts = Map.fromList
      [ (altCon, alt)
      | alt@(XAlt altCon _ _) <- alts
      ]

    altDis (XAlt _ binders rhs) (XAlt _ binders' rhs') =
      [ (binder, binder')
      | (binder, binder') <- disExpr rhs rhs'
      , not $ binder  `Set.member` Set.fromList binders
      , not $ binder' `Set.member` Set.fromList binders'
      ]
