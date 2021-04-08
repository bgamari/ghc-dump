module Modes.Show (showMode) where

import Options.Applicative
import Prettyprinter as PP

import GhcDump.Ast
import GhcDump.Util
import GhcDump.ToHtml
import GhcDump.Pretty (pprModule)

import FilterBindings
import Common

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