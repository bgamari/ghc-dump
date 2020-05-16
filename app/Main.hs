module Main where

import Codec.Serialise (deserialise)
import qualified Data.ByteString.Lazy as BSL
import GhcDump.Ast (SModule)
import System.Environment (getArgs)

import CoreDiff.Diff (someFunc)

main :: IO ()
main = main' =<< getArgs

-- dump single file
main' [path] = do
  mod <- readModFile path
  print mod

-- diff files
{-
main' [pathA, pathB] = do
  ...
-}

main' _      = putStrLn "Incorrect number of arguments, aborting."

readModFile :: FilePath -> IO SModule
readModFile path = deserialise <$> BSL.readFile path
