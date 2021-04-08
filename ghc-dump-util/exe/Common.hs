module Common where

import System.IO (stdout)

import Prettyprinter as PP
import Prettyprinter.Render.Terminal as PP
import qualified System.Console.ANSI

renderIOTerm :: PP.LayoutOptions -> Doc AnsiStyle -> IO ()
renderIOTerm layoutOpts doc = do
    supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout
    let doc' = if supportsANSI
                 then doc
                 else PP.unAnnotate doc
    PP.renderIO stdout $ PP.layoutPretty layoutOpts doc'
