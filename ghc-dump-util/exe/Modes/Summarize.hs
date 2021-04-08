module Modes.Summarize (summarizeMode) where

import Prettyprinter as PP
import Options.Applicative

import GhcDump.Ast
import GhcDump.Util
import GhcDump.ToHtml
import GhcDump.Pretty (pprModule)

import FilterBindings
import Common
import Table

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
