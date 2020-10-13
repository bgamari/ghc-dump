module Main where

import Control.Monad
import Control.Monad.Trans.Reader
import Options.Applicative

import CoreDiff.Util
import CoreDiff.PrettyPrint

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
      print $ runReader (pprWithOpts mod) pprOptsDefault

-- Common argument types
cborDumpFile :: Parser FilePath
cborDumpFile = argument str (metavar "PATH" <> help "ghc-dump CBOR dump")
