module Main where

import Codec.Serialise (deserialise)
import Control.Monad (mapM_)
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Data.List (find)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import GhcDump.Ast
import System.Environment (getArgs)

import CoreDiff.Diff
import CoreDiff.Preprocess
import CoreDiff.PrettyPrint

main :: IO ()
main = main' =<< getArgs

-- diff files on a specific binding
main' [binding, pathA, pathB] = do
  modA <- readModFile pathA
  modB <- readModFile pathB

  let Just (bndrA, _statsA, exprA) = lookupBinding binding modA
  let Just (bndrB, _statsB, exprB) = lookupBinding binding modB

  {-
  putStrLn $ "Binding A (" ++ T.unpack (modulePhase modA) ++ "):"
  print (bndrA, exprA)

  putStrLn $ "Binding B (" ++ T.unpack (modulePhase modB) ++ "):"
  print (bndrB, exprB)
  -}

  let lhs@(ba, _) = (sbinderToBinder [] bndrA, sexprToExpr [] exprA)
  let rhs@(bb, _) = (sbinderToBinder [] bndrB, sexprToExpr [] exprB)


  -- TODO: include global binders
  putStrLn "LHS:"
  print lhs
  let (lhs', lhsBinders) = runState (deBruijnIndex lhs) [ba]
  print lhs'
  putStrLn "RHS:"
  print rhs
  let (rhs', rhsBinders) = runState (deBruijnIndex rhs) [bb]
  print rhs'

  putStrLn "GCP (De-Bruijn indexed terms):"
  let gcp = gcpBinding lhs' rhs'
  putStrLn $ ppr TopLevel gcp

  putStrLn "Fixed GCP (Binder names resubstituted):"
  -- Currently undoDeBruijn favors lhsBinders for binders that appeared in both terms
  -- There is probably a bug in there i'm not seeing
  --
  -- Example: \y.\x.x  //  \y.\a.(a y)
  --          \0.\1.1  //  \0.\1.(1 0)
  -- Would yield:   \y.\x.(x/a y) would show a even tho its not defined
  -- It should be:  \y.\x.(x/x y) or, when rhsBinders is preferred: \y.\a.(a/a y)
  -- Solution: Somehow check for structural binder equality on-the-fly
  -- Alternatively: only substitute the names back, hacky but works
  let allBinders = extendIfShorter lhsBinders rhsBinders
  let gcp' = runReader (undoDeBruijn gcp) (allBinders)
  putStrLn $ ppr TopLevel gcp'

main' _ = putStrLn "Incorrect number of arguments, aborting."


extendIfShorter lb rb = lb ++ drop (length lb) rb


readModFile :: FilePath -> IO SModule
readModFile path = deserialise <$> BSL.readFile path

lookupBinding :: String -> SModule -> Maybe (SBinder, CoreStats, SExpr)
lookupBinding binding mod = find go $ moduleBindings mod
  where go (binder, _, _) = binding == getName binder

getName :: SBinder -> String
getName = T.unpack . binderName . unSBndr
