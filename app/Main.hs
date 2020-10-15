{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Reader
import Data.List
import Data.Maybe
import qualified Data.Set as Set
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

data DiffContext = DiffContext
  { pathA :: FilePath
  , pathB :: FilePath
  , modA :: XModule
  , modB :: XModule
  , inliningMode :: InliningMode
  , displayIdenticalBindings :: Bool
  , opts :: PprOpts
  }

diffCommand = run <$> cborDumpFile <*> cborDumpFile <*> optional inliningOptions <*> displayIdentical
  where
    displayIdentical = switch
      (  long "display-identical"
      <> help "Display identical bindings too."
      )

    run pathA pathB inliningMode displayIdentical = do
      modA <- readXModule pathA
      modB <- readXModule pathB

      run' $ DiffContext
        pathA pathB
        modA modB
        (fromMaybe InliningDisabled inliningMode)
        displayIdentical
        pprOptsDefault

    run' ctx = do
      let modA' = applyInlining (inliningMode ctx) (modA ctx)
      let modB' = applyInlining (inliningMode ctx) (modB ctx)

      let pairings  = pairProg modA' modB'

      let pairings' = permutePaired $ permutePairingsInRhs pairings

      diffBindingByBinding ctx pairings'

    diffBindingByBinding ctx (PairingS _ unpairedL unpairedR paired) = do
      mapM_ (uncurry $ printDiff ctx) paired
      mapM_ (printLeft  ctx) unpairedL
      mapM_ (printRight ctx) unpairedR

    printDiff ctx binding@(XBinding binder _) binding'@(XBinding binder' _) = do
      let binderStr  = show $ runReader (pprWithOpts binder)  $ opts ctx
      let binderStr' = show $ runReader (pprWithOpts binder') $ opts ctx
      -- print $ bold $ text $ "Difference of " ++ binderStr ++ " and " ++ binderStr'
      let bindingStr  = show $ runReader (pprWithOpts binding)  $ opts ctx
      let bindingStr' = show $ runReader (pprWithOpts binding') $ opts ctx
      callDiff
        ctx
        (binderStr  ++ " from " ++ pathA ctx)
        (binderStr' ++ " from " ++ pathB ctx)
        bindingStr bindingStr'

    printLeft ctx binding@(XBinding binder _) = do
      let binderStr  = show $ runReader (pprWithOpts binder)  $ opts ctx
      let bindingStr = show $ runReader (pprWithOpts binding) $ opts ctx
      callDiff
        ctx
        (binderStr  ++ " from " ++ pathA ctx)
        ("No match in " ++ pathB ctx)
        bindingStr ""

    printRight ctx binding@(XBinding binder _) = do
      let binderStr  = show $ runReader (pprWithOpts binder)  $ opts ctx
      let bindingStr = show $ runReader (pprWithOpts binding) $ opts ctx
      callDiff
        ctx
        ("No match in " ++ pathA ctx)
        (binderStr  ++ " from " ++ pathB ctx)
        "" bindingStr
  
    callDiff :: DiffContext -> String -> String -> String -> String -> IO ()
    callDiff ctx labelA labelB a b = do
      withSystemTempFile "corediffa.txt" $ \pathA handleA ->
        withSystemTempFile "corediffb.txt" $ \pathB handleB -> do
          hPutStrLn handleA a
          hFlush handleA
          hPutStrLn handleB b
          hFlush handleB
          -- putStrLn $ diffCmd labelA labelB pathA pathB
          exitCode <- system $ diffCmd labelA labelB pathA pathB
          when (exitCode == ExitSuccess) $ do -- diff returns 0 if the files are the same
            print $ bold $ text $ labelA ++ " and " ++ labelB ++ " are identical"
            when (displayIdenticalBindings ctx) $ do
              putStrLn a
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
    bindersToInline = Set.fromList [ binder | XBinding binder _ <- bindings, pred inliningMode binder ]

    pred InliningDisabled _      = False
    pred InliningEnabled  binder = not $ xBinderIsExported binder
