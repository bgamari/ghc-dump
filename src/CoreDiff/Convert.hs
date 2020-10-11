module CoreDiff.Convert where

import GhcDump.Ast

-- cvtXModule :: Module -> XModule
cvtXModule :: Module -> String
cvtXModule = show
