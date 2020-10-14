{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Reader
import Data.List
import Data.Maybe
import Options.Applicative
import System.IO
import System.IO.Temp
import System.Process
import System.Exit
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import CoreDiff.Assimilate
import CoreDiff.Inline
import CoreDiff.Pairing
import CoreDiff.PrettyPrint
import CoreDiff.Util
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

      let inliningMode' = fromMaybe InliningDisabled inliningMode
      let modA' = applyInlining inliningMode' modA
      let modB' = applyInlining inliningMode' modB

      let pairings  = pairProg modA' modB'
      let pairings' = permutePairingsInRhs pairings

      diffBindingByBinding pathA pathB pprOptsDefault pairings'

    diffBindingByBinding modPath modPath' opts (PairingS _ unpairedL unpairedR paired) = do
      mapM_ (uncurry $ printDiff modPath modPath' opts) paired
      mapM_ (printLeft  modPath modPath' opts) unpairedL
      mapM_ (printRight modPath modPath' opts) unpairedR

    printDiff modPath modPath' opts binding@(XBinding binder _) binding'@(XBinding binder' _) = do
      let binderStr  = show $ runReader (pprWithOpts binder)  opts
      let binderStr' = show $ runReader (pprWithOpts binder') opts
      -- print $ bold $ text $ "Difference of " ++ binderStr ++ " and " ++ binderStr'
      let bindingStr  = show $ runReader (pprWithOpts binding)  opts
      let bindingStr' = show $ runReader (pprWithOpts binding') opts
      callDiff
        (binderStr  ++ " from " ++ modPath)
        (binderStr' ++ " from " ++ modPath')
        bindingStr bindingStr'

    printLeft modPath modPath' opts binding@(XBinding binder _) = do
      let binderStr  = show $ runReader (pprWithOpts binder)  opts
      let bindingStr  = show $ runReader (pprWithOpts binding)  opts
      callDiff
        (binderStr  ++ " from " ++ modPath)
        ("No match in " ++ modPath')
        bindingStr ""

    printRight modPath modPath' opts binding@(XBinding binder _) = do
      let binderStr  = show $ runReader (pprWithOpts binder)  opts
      let bindingStr  = show $ runReader (pprWithOpts binding)  opts
      callDiff
        ("No match in " ++ modPath)
        (binderStr  ++ " from " ++ modPath')
        "" bindingStr
  
    callDiff :: String -> String -> String -> String -> IO ()
    callDiff labelA labelB a b = do
      withSystemTempFile "corediffa.txt" $ \pathA handleA ->
        withSystemTempFile "corediffb.txt" $ \pathB handleB -> do
          hPutStrLn handleA a
          hFlush handleA
          hPutStrLn handleB b
          hFlush handleB
          -- putStrLn $ diffCmd labelA labelB pathA pathB
          exitCode <- system $ diffCmd labelA labelB pathA pathB
          case exitCode of
            ExitSuccess -> -- diff returns 0 if the files are the same
              print $ bold $ text $ labelA ++ " and " ++ labelB ++ " are identical"
            _ -> return ()
      where
        diffCmd labelA labelB pathA pathB = intercalate " "
          [ "diff", "--color=always", "-u"
          , "--label", sQuote labelA
          , pathA
          , "--label", sQuote labelB
          , pathB
          ]
        sQuote str = "'" ++ concat [ if c == '\'' then "'\"'\"'" else [c] | c <- str ] ++ "'"

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
