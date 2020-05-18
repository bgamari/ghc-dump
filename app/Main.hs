module Main where

import Codec.Serialise (deserialise)
import Control.Monad (mapM_)
import Data.List (find)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import GhcDump.Ast
import System.Environment (getArgs)

import CoreDiff.Diff (diffCore)

main :: IO ()
main = main' =<< getArgs

-- diff files on a specific binding
main' [binding, pathA, pathB] = do
  modA <- readModFile pathA
  modB <- readModFile pathB

  let Just (_, _, exprA) = lookupBinding binding modA
  let Just (_, _, exprB) = lookupBinding binding modB

  print exprA
  print exprB

  -- let patch = diffCore exprA exprB

  -- print patch

main' _      = putStrLn "Incorrect number of arguments, aborting."

readModFile :: FilePath -> IO SModule
readModFile path = deserialise <$> BSL.readFile path

printMod :: SModule -> IO ()
printMod mod = do
  putStrLn $ "module " ++ (T.unpack $ showModName $ moduleName mod) ++ "[" ++ (T.unpack $ modulePhase mod) ++  "] where"
  mapM_ print $ moduleTopBindings mod

  where
    showModName = getModuleName

lookupBinding :: String -> SModule -> Maybe (SBinder, CoreStats, SExpr)
lookupBinding binding mod = find go $ moduleBindings mod
  where go (binder, _, _) = binding == getName binder

getName :: SBinder -> String
getName = T.unpack . binderName . unSBndr
