module Main where

import Control.Monad
import Control.Monad.Trans.Reader
import Options.Applicative

import CoreDiff.Util
import CoreDiff.Pairing
import CoreDiff.PrettyPrint

main :: IO ()
main = join $ execParser $ info (helper <*> commands) mempty

commands :: Parser (IO ())
commands = subparser $ foldl (<>) mempty
  [ command "show" (info (helper <*> showCommand) (progDesc "Print a ghc-dump CBOR dump"))
  , command "pair" (info (helper <*> pairCommand) (progDesc "Show the top level pairings for two modules"))
  ]

showCommand = run <$> cborDumpFile
  where
    run filePath = do
      mod <- readXModule filePath
      print $ runReader (pprWithOpts mod) pprOptsDefault

pairCommand = run <$> cborDumpFile <*> cborDumpFile
  where
    run pathA pathB = do
      modA <- readXModule pathA
      modB <- readXModule pathB

      let pairings = pairProg modA modB
      print $ runReader (pprWithOpts pairings) pprOptsDefault

-- Common argument types
cborDumpFile :: Parser FilePath
cborDumpFile = argument str (metavar "PATH" <> help "ghc-dump CBOR dump")
