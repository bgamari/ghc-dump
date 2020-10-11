module Main where

import Control.Monad
import Options.Applicative

import CoreDiff.Util

main :: IO ()
main = join $ execParser $ info (helper <*> commands) mempty

commands :: Parser (IO ())
commands = subparser $ foldl (<>) mempty
  [ command "show" (info (helper <*> showCommand) (progDesc "Print a ghc-dump CBOR dump"))
  ]

showCommand = run <$> cborDumpFile
  where
    run filePath = do
      mod <- readXModule filePath
      print mod

-- Common argument types
cborDumpFile :: Parser FilePath
cborDumpFile = argument str (metavar "PATH" <> help "ghc-dump CBOR dump")
