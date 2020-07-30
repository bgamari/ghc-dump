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

  -- TODO: this is already implemented in ghc-dump-util. (reconModule)
  let lhs@(XAst.XBinding lhsBinder _) = XAst.cvtBinding (sbinderToBinder [] bndrA, sexprToExpr [] exprA)
  let rhs@(XAst.XBinding rhsBinder _) = XAst.cvtBinding (sbinderToBinder [] bndrB, sexprToExpr [] exprB)

  let (dbLhs, lhsBinders) = runState (deBruijnIndex lhs) [lhsBinder]
  let (dbRhs, rhsBinders) = runState (deBruijnIndex rhs) [rhsBinder]

  putStrLn "Left binder after De-Bruijn:"
  print $ runReader (ppr dbLhs) pprDefaultOpts
  putStrLn "Right binder after De-Bruijn:"
  print $ runReader (ppr dbRhs) pprDefaultOpts

  putStrLn "GCP:"
  print $ runReader (ppr $ gcpBinding dbLhs dbRhs) pprDefaultOpts


main' _ = putStrLn "Incorrect number of arguments, aborting."


extendIfShorter lb rb = lb ++ drop (length lb) rb


readModFile :: FilePath -> IO SModule
readModFile path = deserialise <$> BSL.readFile path

lookupBinding :: String -> SModule -> Maybe (SBinder, CoreStats, SExpr)
lookupBinding binding mod = find go $ moduleBindings mod
  where go (binder, _, _) = binding == getName binder

getName :: SBinder -> String
getName = T.unpack . binderName . unSBndr
