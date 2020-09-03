{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Codec.Serialise (deserialise)
import Control.Monad (mapM_)
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import GhcDump.Ast
import GhcDump.Util
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.PrettyPrint.ANSI.Leijen

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

  let bindingsA = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modA
  let bindingsB = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modB

  print $ bold $ text $ "Binders in " ++ pathA ++ ":"
  printBinderNames bindingsA
  print $ bold $ text $ "Binders in " ++ pathB ++ ":"
  printBinderNames bindingsB

  print $ bold $ text $ "Unique binder names in " ++ pathA ++ ":"
  printBinderNameCounts bindingsA
  print $ bold $ text $ "Unique binder names in " ++ pathB ++ ":"
  printBinderNameCounts bindingsB

  print $ bold $ red $ text $ "Floating in..."

  let bindingsAFloatedIn = floatInTopLvl bindingsA
  let bindingsBFloatedIn = floatInTopLvl bindingsB

  print $ bold $ text $ "Unique binder names in " ++ pathA ++ " after float-in:"
  printBinderNameCounts bindingsAFloatedIn
  print $ bold $ text $ "Unique binder names in " ++ pathB ++ " after float-in:"
  printBinderNameCounts bindingsBFloatedIn

  print $ bold $ text $ "Bindings in " ++ pathA ++ " after float-in:"
  printBindings bindingsAFloatedIn
  print $ bold $ text $ "Bindings in " ++ pathB ++ " after float-in:"
  printBindings bindingsBFloatedIn
  
  let pairings = findPairings bindingsAFloatedIn bindingsBFloatedIn
  printPairings pairings

main' ["diffmod", pathA, pathB] = do
  modA <- readDump pathA
  modB <- readDump pathB

  let bindingsA = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modA
  let bindingsB = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modB

  -- print $ bold $ red $ text $ "Floating in..."

  let bindingsAFloatedIn = floatInTopLvl bindingsA
  let bindingsBFloatedIn = floatInTopLvl bindingsB
  
  let pairings = findPairings bindingsAFloatedIn bindingsBFloatedIn
  printPairingDiffs pairings

main' ["diffmod2", pathA, pathB, diffA, diffB] = do
  modA <- readDump pathA
  modB <- readDump pathB

  print $ bold $ text $ "Comparing " ++ T.unpack (modulePhase modA) ++ " and " ++ T.unpack (modulePhase modB) ++ "..."

  let bindersA = map (fst . ignoreStats) $ moduleBindings modA
  let bindersB = map (fst . ignoreStats) $ moduleBindings modB

  let bindingsA = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modA
  let bindingsB = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modB

  let bindingsAFloatedIn = floatInTopLvl bindingsA
  let bindingsBFloatedIn = floatInTopLvl bindingsB

  let pairings = findPairings bindingsAFloatedIn bindingsBFloatedIn

  printPairingDiffs' pairings diffA diffB

main' ["diffmod3", pathA, pathB] = do
  modA <- readDump pathA
  modB <- readDump pathB
  let phaseA = T.unpack $ modulePhase modA
  let phaseB = T.unpack $ modulePhase modB

  print $ bold $ text $ "Comparing " ++ phaseA ++ " and " ++ phaseB ++ "..."

  let bindingsA = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modA
  let bindingsB = map (XAst.cvtBinding . ignoreStats) $ moduleBindings modB

  let bindingsAFloatedIn = floatInTopLvl bindingsA
  let bindingsBFloatedIn = floatInTopLvl bindingsB

  let s = snaadInit bindingsAFloatedIn bindingsBFloatedIn
  print s
  snaadInteractive s
  where
    snaadInteractive s = do
      -- TODO: short status line
      cmd <- prompt "> "
      handleCmd s $ splitOn ' ' cmd

    handleCmd s (cmd:args) =
      | cmd `elem` ["step", "s"] = do
        let (_done, s') = runState snaadStep s
        print s'
        snaadInteractive s'
      | cmd `elem` ["continue", "c"] = do
        let s' = stepUntilDone s
        print s'
        snaadInteractive s'
      | cmd `elem` ["print1", "p1"] = do
        print1 s
        snaadInteractive s

    handleCmd s _ = snaadInteractive s

    stepUntilDone s =
      case runState snaadStep s of
        (True, s') -> s'
        (False, s') -> stepUntilDone s'

    print1 s = mapM printChg $ map (uncurry diff) $ snaadPairs s
      where
        printChg c =
          print $ runReader (ppr c) pprDefaultOpts

    -- helpers

    prompt str = do
      putStr str
      hFlush stdout
      getLine

    splitOn d list = splitOn' [] d list
      where
        splitOn' acc d [] = [reverse acc]
        splitOn' acc d (x:xs)
          | d == x    = reverse acc : splitOn' [] d xs
          | otherwise = splitOn' (x:acc) d xs


main' _ = putStrLn "Incorrect number of arguments, aborting."


lookupBinding :: String -> Module -> Maybe (Binder, CoreStats, Expr)
lookupBinding binding mod = find go $ moduleBindings mod
  where go (binder, _, _) = binding == getName binder

getName :: Binder -> String
getName = T.unpack . binderName . unBndr

ignoreStats (binder, _stats, expr) = (binder, expr)

printPairings = mapM_ go
  where
    go (Both l r) = putStrLn $ "Both: (" ++ show (runReader (ppr $ xb l) opts) ++ "," ++ show (runReader (ppr $ xb r) opts) ++ ")"
    go (OnlyLeft l) = do
      putStrLn $ "Left: " ++ show (red $ runReader (ppr $ xb l) opts)
      putStrLn $ "Dbg : " ++ show (xb l)
    go (OnlyRight l) = do
      putStrLn $ "Right: " ++ show (green $ runReader (ppr $ xb l) opts)
      putStrLn $ "Dbg  : " ++ show (xb l)

    opts = pprDefaultOpts { pprShowIdInfo = True }
    xb (XAst.XBinding b _) = b

printPairingDiffs pairings = mapM_ go pairings
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
        r' = runReader (swapNames l r) associatedBinderNames

    opts = pprDefaultOpts { pprShowIdInfo = True }
    xb (XAst.XBinding b _) = b
    associatedBinderNames =
      [ (getUName lBinder, getUName rBinder)
      | Both (XAst.XBinding lBinder _) (XAst.XBinding rBinder _) <- pairings
      ]

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
        r' = runReader (swapNames l r) associatedBinderNames

    ppr' x = runReader (ppr x) pprDefaultOpts
    associatedBinderNames =
      [ (getUName lBinder, getUName rBinder)
      | Both (XAst.XBinding lBinder _) (XAst.XBinding rBinder _) <- pairings
      ]

printBinderNames :: [XAst.XBinding XAst.UD] -> IO ()
printBinderNames bindings =
  print $ map go bindings
  where
    go (XAst.XBinding binder _)= runReader (ppr binder) opts
    opts = pprDefaultOpts { pprShowIdInfo = False }

printBinderNameCounts :: [XAst.XBinding XAst.UD] -> IO ()
printBinderNameCounts bindings = do
  mapM_ printRow $ sort binderCounts
  where
    binderNames = map (\(XAst.XBinding bndr _) -> XAst.xBinderName bndr) bindings
    binderNameSet = Set.fromList binderNames
    binderCounts = [(count name binderNames, name) | name <- Set.toList binderNameSet]
    count x list = length $ filter (== x) list

    printRow (count, name) =
      putStrLn $ show count ++ "\t" ++ T.unpack name

printBindings :: [XAst.XBinding XAst.UD] -> IO ()
printBindings = print . vsep . intersperse hardline . map go
  where go binding = runReader (ppr binding) pprDefaultOpts
