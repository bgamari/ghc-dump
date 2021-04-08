{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Maybe
import Data.List (sortBy)
import Data.Ord
import System.IO (stdout)

import Options.Applicative
import Prettyprinter as PP
import Prettyprinter.Render.Terminal as PP
import qualified System.Console.ANSI

import Text.Regex.TDFA
import Text.Regex.TDFA.Common (Regex)
import Text.Regex.TDFA.Text ()

import GhcDump.ToHtml
import GhcDump.Pretty
import GhcDump.Util
import GhcDump.Ast

import FilterBindings
import Common
import Table

type Mode = Parser (IO ())

modes :: Mode
modes = subparser
     $ mode "show" showMode (progDesc "print Core")
    <> mode "list-bindings" listBindingsMode (progDesc "list top-level bindings, their sizes, and types")
    <> mode "summarize" summarizeMode (progDesc "summarize multiple dump files")

mode :: String -> Parser a -> InfoMod a -> Mod CommandFields a
mode name f opts = command name (info (helper <*> f) opts)

dumpFile :: Parser FilePath
dumpFile = argument str (metavar "DUMP FILE" <> help "CBOR dump file")

prettyOpts :: Parser PrettyOpts
prettyOpts =
    PrettyOpts
        <$> switch (short 'u' <> long "show-uniques" <> help "Show binder uniques")
        <*> switch (short 'i' <> long "show-idinfo" <> help "Show IdInfo of bindings")
        <*> switch (short 'T' <> long "show-let-types" <> help "Show type signatures for let-bound binders")
        <*> switch (short 'U' <> long "show-unfoldings" <> help "Show unfolding templates")

showMode :: Mode
showMode =
    run <$> filterCond <*> bindingsSort <*> prettyOpts <*> html <*> dumpFile
    where
    html = switch (short 'H' <> long "html" <> help "Render to HTML")
    run filterFn sortBindings opts html fname = do
        dump <- sortBindings . filterFn <$> GhcDump.Util.readDump fname
        if html
            then writeFile "out.html" $ show $ topBindingsToHtml (moduleTopBindings dump)
            else renderIOTerm PP.defaultLayoutOptions $ pprModule opts dump

listBindingsMode :: Mode
listBindingsMode =
    run <$> filterCond <*> bindingsSort <*> prettyOpts <*> dumpFile
    where
    run filterFn sortBindings opts fname = do
        dump <- sortBindings . filterFn <$> GhcDump.Util.readDump fname
        let table = [ Col 30 "Name"   (pprBinder opts . getBinder)
                    , Col 6  "Terms"  (pretty . csTerms . getStats)
                    , Col 6  "Types"  (pretty . csTypes . getStats)
                    , Col 6  "Coerc." (pretty . csCoercions . getStats)
                    , Col 3000 "Type"  (pprType opts . binderType . unBndr . getBinder)
                    ]
        let layoutOpts = PP.defaultLayoutOptions { layoutPageWidth = Unbounded }
        renderIOTerm layoutOpts $ vcat
            [ pretty (modulePhase dump)
            , renderTable table (moduleBindings dump)
            ]

summarizeMode :: Mode
summarizeMode =
    run <$> some dumpFile
    where
    run fnames = do
        mods <- mapM (\fname -> do mod <- readDump fname
                                   return (fname, mod)) fnames
        let totalSize :: Module -> CoreStats
            totalSize = foldMap getStats . moduleBindings
        let table = [ Col 35 "Name" (pretty . fst)
                    , Col 8  "Terms" (pretty . csTerms . totalSize . snd)
                    , Col 8  "Types" (pretty . csTypes . totalSize . snd)
                    , Col 8  "Coerc." (pretty . csCoercions . totalSize . snd)
                    , Col 35 "Previous phase" (pretty . modulePhase . snd)
                    ]
        print $ renderTable table mods

getBinder (b,_,_) = b
getStats (_,s,_) = s
getRHS (_,_,e) = e

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
