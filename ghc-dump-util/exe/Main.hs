import Control.Monad
import Options.Applicative

import Common

import Modes.Show
import Modes.ListBindings
import Modes.Summarize

modes :: Mode
modes = subparser
     $ mode "show" showMode (progDesc "print Core")
    <> mode "list-bindings" listBindingsMode (progDesc "list top-level bindings, their sizes, and types")
    <> mode "summarize" summarizeMode (progDesc "summarize multiple dump files")

mode :: String -> Parser a -> InfoMod a -> Mod CommandFields a
mode name f opts = command name (info (helper <*> f) opts)

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
