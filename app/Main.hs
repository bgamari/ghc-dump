module Main where

import Codec.Serialise (deserialise)
import Control.Monad (mapM_)
import Data.List (find)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import GhcDump.Ast
import System.Environment (getArgs)

import CoreDiff.Diff (strExpr, diffExpr)

main :: IO ()
main = main' =<< getArgs

-- diff files on a specific binding
main' [binding, pathA, pathB] = do
  modA <- readModFile pathA
  modB <- readModFile pathB

  let Just (bndrA, _, exprA) = lookupBinding binding modA
  let Just (bndrB, _, exprB) = lookupBinding binding modB

  {-
  putStrLn $ "Binder of " ++ binding ++ " in " ++ pathA ++ ":"
  print bndrA
  putStrLn $ "Binder of " ++ binding ++ " in " ++ pathB ++ ":"
  print bndrB
  -}

  let patch = diffExpr (strExpr exprA) (strExpr exprB)
  print patch

main' _ = putStrLn "Incorrect number of arguments, aborting."

readModFile :: FilePath -> IO SModule
readModFile path = deserialise <$> BSL.readFile path

lookupBinding :: String -> SModule -> Maybe (SBinder, CoreStats, SExpr)
lookupBinding binding mod = find go $ moduleBindings mod
  where go (binder, _, _) = binding == getName binder

getName :: SBinder -> String
getName = T.unpack . binderName . unSBndr
