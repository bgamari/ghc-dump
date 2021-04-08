module Modes.ListBindings (listBindingsMode) where

import Options.Applicative
import Prettyprinter as PP

import GhcDump.Ast
import GhcDump.Util
import GhcDump.ToHtml
import GhcDump.Pretty

import FilterBindings
import Common
import Table

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

getBinder (b,_,_) = b
getStats (_,s,_) = s
getRHS (_,_,e) = e
