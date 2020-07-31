{-# LANGUAGE FlexibleContexts #-}

module Main where

import Codec.Serialise (deserialise)
import Control.Monad (mapM_)
import Control.Monad.Reader
import Control.Monad.State
import Data.List (find)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import GhcDump.Ast
import System.Environment (getArgs)

import qualified CoreDiff.XAst as XAst
import CoreDiff.Diff
import CoreDiff.Preprocess
import CoreDiff.PrettyPrint

main :: IO ()
main = main' =<< getArgs

-- diff files on a specific binding
main' ["debruijn", binding, pathA, pathB] = do
  modA <- readModFile pathA
  modB <- readModFile pathB

  let Just bndgA = lookupBinding binding modA
  let Just bndgB = lookupBinding binding modB

  {-
  putStrLn $ "Binding A (" ++ T.unpack (modulePhase modA) ++ "):"
  print (bndrA, exprA)

  putStrLn $ "Binding B (" ++ T.unpack (modulePhase modB) ++ "):"
  print (bndrB, exprB)
  -}

  -- TODO: this is already implemented in ghc-dump-util. (reconModule)
  let lhs@(XAst.XBinding lhsBinder _) = XAst.cvtBinding (sbindingToBinding [] $ ignoreStats bndgA)
  let rhs@(XAst.XBinding rhsBinder _) = XAst.cvtBinding (sbindingToBinding [] $ ignoreStats bndgB)

  let (dbLhs, lhsBinders) = runState (deBruijnIndex lhs) [lhsBinder]
  let (dbRhs, rhsBinders) = runState (deBruijnIndex rhs) [rhsBinder]

  putStrLn "Left binder after De-Bruijn:"
  print $ runReader (ppr dbLhs) pprDefaultOpts
  putStrLn "Right binder after De-Bruijn:"
  print $ runReader (ppr dbRhs) pprDefaultOpts

  putStrLn "Diff:"
  print $ runReader (ppr $ diff dbLhs dbRhs) pprDefaultOpts

main' ["pairings", pathA, pathB] = do
  modA <- readModFile pathA
  modB <- readModFile pathB

  let bindersA = map (sbinderToBinder [] . fst . ignoreStats) $ moduleBindings modA
  let bindersB = map (sbinderToBinder [] . fst . ignoreStats) $ moduleBindings modB

  print $ map (\b -> (binderUniqueName b, binderType $ unBndr b)) bindersA
  print $ map (\b -> (binderUniqueName b, binderType $ unBndr b)) bindersB

  let bindingsA = map (XAst.cvtBinding . sbindingToBinding bindersA . ignoreStats) $ moduleBindings modA
  let bindingsB = map (XAst.cvtBinding . sbindingToBinding bindersB . ignoreStats) $ moduleBindings modB
  
  let pairings = findPairings bindingsA bindingsB

  printPairings pairings

main' ["diffmod", pathA, pathB] = do
  modA <- readModFile pathA
  modB <- readModFile pathB

  let bindersA = map (sbinderToBinder [] . fst . ignoreStats) $ moduleBindings modA
  let bindersB = map (sbinderToBinder [] . fst . ignoreStats) $ moduleBindings modB

  let bindingsA = map (XAst.cvtBinding . sbindingToBinding bindersA . ignoreStats) $ moduleBindings modA
  let bindingsB = map (XAst.cvtBinding . sbindingToBinding bindersB . ignoreStats) $ moduleBindings modB
  
  let pairings = findPairings bindingsA bindingsB

  printPairingDiffs pairings

main' _ = putStrLn "Incorrect number of arguments, aborting."


readModFile :: FilePath -> IO SModule
readModFile path = deserialise <$> BSL.readFile path

lookupBinding :: String -> SModule -> Maybe (SBinder, CoreStats, SExpr)
lookupBinding binding mod = find go $ moduleBindings mod
  where go (binder, _, _) = binding == getName binder

getName :: SBinder -> String
getName = T.unpack . binderName . unSBndr

ignoreStats (binder, _stats, expr) = (binder, expr)

-- TODO: this is just for debugging
printPairings = mapM_ go
  where
    go (Both l r) = putStrLn $ "Both: (" ++ show (runReader (ppr $ xb l) opts) ++ "," ++ show (runReader (ppr $ xb r) opts) ++ ")"
    go (OnlyLeft l) = putStrLn $ "Left: " ++ show (runReader (ppr $ xb l) opts)
    go (OnlyRight l) = putStrLn $ "Right: " ++ show (runReader (ppr $ xb l) opts)

    opts = pprDefaultOpts { pprShowIdInfo = False }
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
      putStrLn "Both (DB-d):"
      print $ runReader (ppr $ diff dbL dbR) opts
      where
        (dbL, _) = runState (deBruijnIndex l) []
        (dbR, _) = runState (deBruijnIndex r) []

    opts = pprDefaultOpts { pprShowIdInfo = True }
    xb (XAst.XBinding b _) = b
