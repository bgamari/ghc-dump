module Main where

import Control.Monad
import Control.Monad.Trans.Reader
import Data.List
import Data.Maybe
import Options.Applicative

import CoreDiff.Util
import CoreDiff.Inline
import CoreDiff.Pairing
import CoreDiff.PrettyPrint
import CoreDiff.XAst

main :: IO ()
main = join $ execParser $ info (helper <*> commands) mempty

commands :: Parser (IO ())
commands = subparser $ foldl (<>) mempty
  [ command "show" (info (helper <*> showCommand) (progDesc "Print a ghc-dump CBOR dump"))
  , command "pair" (info (helper <*> pairCommand) (progDesc "Show the top level pairings for two modules"))
  , command "diff" (info (helper <*> diffCommand) (progDesc "Difference two modules"))
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

diffCommand = run <$> cborDumpFile <*> cborDumpFile <*> optional inliningOptions
  where
    run pathA pathB inliningMode = do
      modA <- readXModule pathA
      modB <- readXModule pathB

      let inliningMode' = fromMaybe InliningEnabled inliningMode
      let modA' = applyInlining inliningMode' modA
      let modB' = applyInlining inliningMode' modB

      print $ runReader (pprWithOpts modA') pprOptsDefault

data InliningMode
  = InliningEnabled
  | InliningDisabled
      

-- Common argument types
--
cborDumpFile :: Parser FilePath
cborDumpFile = argument str (metavar "PATH" <> help "ghc-dump CBOR dump")

-- TODO: is there a better way to have a default?
inliningOptions :: Parser InliningMode
inliningOptions = inliningEnabled <|> inliningDisabled
  where
    inliningEnabled = flag' InliningEnabled
      (  long "inlining-on"
      <> help "Enable inlining for any non-exported binders (default)."
      )

    inliningDisabled = flag' InliningDisabled
      (  long "inlining-off"
      <> help "Disable inlining completely."
      )

-- Implementation stuff that is to superficial for the library.
applyInlining inliningMode mod =
  mod { xModuleBindings = inline bindersToInline bindings }
  where
    bindings = xModuleBindings mod
    bindersToInline = [ binder | XBinding binder _ <- bindings, pred inliningMode binder ]

    pred InliningDisabled _      = False
    pred InliningEnabled  binder = not $ xBinderIsExported binder
