{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Codec.Serialise (deserialise)
import Control.Monad (mapM_)
import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Data.List
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import GhcDump.Ast
import GhcDump.Util
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import CoreDiff.XAst
import CoreDiff.Diff
import CoreDiff.Preprocess
import CoreDiff.PrettyPrint

-- TODO: clean up this mess, its horrible

main :: IO ()
main = main' =<< getArgs

bindingsOf :: Module -> [XBinding UD]
bindingsOf = map cvtBinding . map ignoreStats . moduleBindings

main' ["phase", path] = do
  mod <- readDump path
  print $ T.unpack $ modulePhase mod

main' ["signatures", path] = do
  mod <- readDump path
  let bindings = bindingsOf mod
  let sigs = map signature $ map xBindingBinder $ bindings
  let names = map xBinderName $ map xBindingBinder $ bindings

  print $ vcat $ map pretty $ sort $ nub sigs
  print $ pretty $ count names
  print $ T.unpack $ modulePhase mod
  putStrLn $ "Number of bindings: " ++ show (length bindings)
  putStrLn $ "Distinct signatures: " ++ show (length $ nub sigs)
  putStrLn $ "Distinct names: " ++ show (length $ nub names)


main' ["detectfo", pathRef, path] = do
  modRef <- readDump pathRef
  mod <- readDump path
  let bindingsRef = bindingsOf modRef
  let bindings = bindingsOf mod

  let bindersRef = Set.fromList $ map xBindingBinder bindingsRef
  let binders = Set.fromList $ map xBindingBinder bindings

  let newBinders = Set.difference binders bindersRef
  
  print $ (bold $ text $ getPhase modRef) <+> text "vs." <+> (bold $ text $ getPhase mod)
  putStrLn $ "Newly introduced global binders (" ++ show (length newBinders) ++ "):"
  print $ pretty $ count $ map xBinderName $ Set.toList newBinders

  print $ bold $ text "Usage in binders (new binders blue):"
  print $ vsep
    [ hang 2 $ vsep $ (pprBndr newBinders b <> colon) : (map (pprBndr newBinders) $ Set.toList floatedOut)
    | (b, floatedOut) <- map (floatOutInfo newBinders) bindings
    -- , length floatedOut > 0
    ]

  where
    pprBndr spesh b = (if Set.member b spesh then blue else id) (runReader (ppr b) opts)
      where opts = pprDefaultOpts { pprShowIdInfo = False, pprShowTypes = False }

    floatOutInfo newBinders (XBinding b e) =
      (b, Set.intersection newBinders $ Set.union (fv b) (fv e))

main' ["binders", path] = do
  bindings <- bindingsOf <$> readDump path
  
  print $ vsep
    [ runReader (ppr binder) opts
    | XBinding binder _ <- bindings
    ]

  where
    opts = pprDefaultOpts { pprShowTypes = False, pprShowIdInfo = False }

main' ["ppr", name, path] = do
  bindings <- bindingsOf <$> readDump path

  print $ vsep
    [ runReader (ppr binding) opts
    | binding <- bindings
    , xBinderName (xBindingBinder binding) == T.pack name
    ]
  where
    opts = pprDefaultOpts -- { pprShowUniques = False }

main' ["pairings", pathA, pathB] = do
  modA <- readDump pathA
  modB <- readDump pathB

  let bindingsA = map (cvtBinding . ignoreStats) $ moduleBindings modA
  let bindingsB = map (cvtBinding . ignoreStats) $ moduleBindings modB

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

main' ["diffmod2", pathA, pathB, diffA, diffB] = do
  modA <- readDump pathA
  modB <- readDump pathB

  print $ bold $ text $ "Comparing " ++ T.unpack (modulePhase modA) ++ " and " ++ T.unpack (modulePhase modB) ++ "..."

  let bindersA = map (fst . ignoreStats) $ moduleBindings modA
  let bindersB = map (fst . ignoreStats) $ moduleBindings modB

  let bindingsA = map (cvtBinding . ignoreStats) $ moduleBindings modA
  let bindingsB = map (cvtBinding . ignoreStats) $ moduleBindings modB

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

  let bindingsA = map (cvtBinding . ignoreStats) $ moduleBindings modA
  let bindingsB = map (cvtBinding . ignoreStats) $ moduleBindings modB

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

    handleCmd s (cmd:args)
      | cmd `elem` ["step", "s"] = do
        let (_, s') = runState snaadStep s
        print s'
        snaadInteractive s'
      | cmd `elem` ["continue", "c"] = do
        let (_, s') = runState stepUntilDone s
        print s'
        snaadInteractive s'
      | cmd `elem` ["print1", "p1"] = do
        print1 s
        snaadInteractive s

    handleCmd s _ = snaadInteractive s

    stepUntilDone = do
      done <- snaadStep
      if done then return () else stepUntilDone

    print1 s = mapM printChg $ getPairs s
      where
        printChg (lhs@(XBinding bndr _), rhs)
          -- TODO: implement and use prettyprinting for XBinderUniqueName
          | lhs == rhs = putStrLn $ "No difference in " ++ show (xBinderUName bndr)
          | otherwise =
            print $ runReader (ppr (diff lhs rhs)) pprDefaultOpts

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
    xb (XBinding b _) = b


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
      [ (xBinderUName lBinder, xBinderUName rBinder)
      | Both (XBinding lBinder _) (XBinding rBinder _) <- pairings
      ]

printBinderNames :: [XBinding UD] -> IO ()
printBinderNames bindings =
  print $ map go bindings
  where
    go (XBinding binder _)= runReader (ppr binder) opts
    opts = pprDefaultOpts { pprShowIdInfo = False }

printBinderNameCounts :: [XBinding UD] -> IO ()
printBinderNameCounts bindings = do
  mapM_ printRow $ sort binderCounts
  where
    binderNames = map (\(XBinding bndr _) -> xBinderName bndr) bindings
    binderNameSet = Set.fromList binderNames
    binderCounts = [(count name binderNames, name) | name <- Set.toList binderNameSet]
    count x list = length $ filter (== x) list

    printRow (count, name) =
      putStrLn $ show count ++ "\t" ++ T.unpack name

printBindings :: [XBinding UD] -> IO ()
printBindings = print . vsep . intersperse hardline . map go
  where go binding = runReader (ppr binding) pprDefaultOpts

newtype Count a = Count (Map.Map a Int)

count :: Ord a => [a] -> Count a
count = foldl go (Count Map.empty)
  where
    go (Count m) x =
      Count $ Map.alter f x m

    f Nothing  = Just 1
    f (Just x) = Just $ x + 1

getPhase :: Module -> String
getPhase = takeWhile (/= ':') . T.unpack . modulePhase

instance Pretty a => Pretty (Count a) where
  pretty (Count m) =
    vcat $ map go $ sortBy (compare `on` snd) $ Map.toList m
    where
      go (k, n) = pretty k <> colon <+> pretty n
