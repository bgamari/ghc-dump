{-# LANGUAGE FlexibleContexts #-}

module Main where

import Codec.Serialise (deserialise)
import Control.Monad (mapM_)
import Control.Monad.Reader
import Control.Monad.State
import Data.List (find, intercalate)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import GhcDump.Ast
import GhcDump.Util
import System.Environment (getArgs)
import Text.PrettyPrint.ANSI.Leijen (red, green)

import qualified CoreDiff.XAst as XAst
import CoreDiff.Diff
import CoreDiff.Preprocess
import CoreDiff.PrettyPrint

-- TODO: clean up this mess, its horrible

main :: IO ()
main = main' =<< getArgs

-- diff files on a specific binding
main' ["debruijn", binding, pathA, pathB] = do
  modA <- readDump pathA
  modB <- readDump pathB

  let Just bndgA = lookupBinding binding modA
  let Just bndgB = lookupBinding binding modB

  {-
  putStrLn $ "Binding A (" ++ T.unpack (modulePhase modA) ++ "):"
  print (bndrA, exprA)

  putStrLn $ "Binding B (" ++ T.unpack (modulePhase modB) ++ "):"
  print (bndrB, exprB)
  -}

  -- TODO: this is already implemented in ghc-dump-util. (reconModule)
  let lhs@(XAst.XBinding lhsBinder _) = XAst.cvtBinding $ ignoreStats bndgA
  let rhs@(XAst.XBinding rhsBinder _) = XAst.cvtBinding $ ignoreStats bndgB

  let (dbLhs, lhsBinders) = runState (deBruijnIndex lhs) [lhsBinder]
  let (dbRhs, rhsBinders) = runState (deBruijnIndex rhs) [rhsBinder]

  putStrLn "Left binder after De-Bruijn:"
  print $ runReader (ppr dbLhs) pprDefaultOpts
  putStrLn "Right binder after De-Bruijn:"
  print $ runReader (ppr dbRhs) pprDefaultOpts

  putStrLn "Diff:"
  print $ runReader (ppr $ diff dbLhs dbRhs) pprDefaultOpts

main' ["pairings", pathA, pathB] = do
  modA <- readDump pathA
  modB <- readDump pathB

  let bindersA = map (fst . ignoreStats) $ moduleBindings modA
  let bindersB = map (fst . ignoreStats) $ moduleBindings modB

  let bindingsA = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modA
  let bindingsB = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modB
  
  let pairings = findPairings bindingsA bindingsB

  printPairings pairings

main' ["diffmod", pathA, pathB] = do
  modA <- readDump pathA
  modB <- readDump pathB

  let bindersA = map (fst . ignoreStats) $ moduleBindings modA
  let bindersB = map (fst . ignoreStats) $ moduleBindings modB

  let bindingsA = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modA
  let bindingsB = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modB
  
  let pairings = findPairings bindingsA bindingsB

  printPairingDiffs pairings

main' ["diffmod2", pathA, pathB, diffA, diffB] = do
  modA <- readDump pathA
  modB <- readDump pathB

  let bindersA = map (fst . ignoreStats) $ moduleBindings modA
  let bindersB = map (fst . ignoreStats) $ moduleBindings modB

  let bindingsA = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modA
  let bindingsB = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modB

  let pairings = findPairings bindingsA bindingsB

  printPairingDiffs' pairings diffA diffB

main' _ = putStrLn "Incorrect number of arguments, aborting."


lookupBinding :: String -> Module -> Maybe (Binder, CoreStats, Expr)
lookupBinding binding mod = find go $ moduleBindings mod
  where go (binder, _, _) = binding == getName binder

getName :: Binder -> String
getName = T.unpack . binderName . unBndr

ignoreStats (binder, _stats, expr) = (binder, expr)

-- TODO: this is just for debugging
printPairings = mapM_ go
  where
    go (Both l r) = putStrLn $ "Both: (" ++ show (runReader (ppr $ xb l) opts) ++ "," ++ show (runReader (ppr $ xb r) opts) ++ ")"
    go (OnlyLeft l) = putStrLn $ "Left: " ++ show (red $ runReader (ppr $ xb l) opts)
    go (OnlyRight l) = putStrLn $ "Right: " ++ show (green $ runReader (ppr $ xb l) opts)

    opts = pprDefaultOpts { pprShowIdInfo = True }
    xb (XAst.XBinding b _) = b

printPairingDiffs = mapM_ go
  where
    go (OnlyLeft l) = do
      putStrLn "Left only: "
      print $ runReader (ppr l) opts
    go (OnlyRight l) = do
      putStrLn "Right only:"
      print $ runReader (ppr l) opts
    go (Both l r) = do
      putStrLn "Both (assimilated):"
      print $ runReader (ppr $ diff l r') opts
      where
        r' = swapNamesTopLvl l r

    opts = pprDefaultOpts { pprShowIdInfo = True }
    xb (XAst.XBinding b _) = b

printPairingDiffs' pairings diffL diffR = do
  writeFile diffL $ intercalate "\n" $ reverse lStrs
  writeFile diffR $ intercalate "\n" $ reverse rStrs
  where
    (lStrs, rStrs) = foldl go ([], []) pairings
    go (accL, accR) (OnlyLeft l)  = (accL ++ [show $ ppr' l], accR)
    go (accL, accR) (OnlyRight r) = (accL, accR ++ [show $ ppr' r])
    go (accL, accR) (Both l r)    =
      ( accL ++ [show (ppr' l)]
      , accR ++ [show (ppr' r')]
      )
      where
        r' = swapNamesTopLvl l r

    ppr' x = runReader (ppr x) pprDefaultOpts
