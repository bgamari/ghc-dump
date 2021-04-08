module Common
    ( -- * Pretty-printing utilities
      renderIOTerm

      -- * Command-line parsing
    , Mode
    , prettyOpts
    , dumpFile
    ) where

import System.IO (stdout)

import Prettyprinter as PP
import Prettyprinter.Render.Terminal as PP
import qualified System.Console.ANSI
import Options.Applicative

import GhcDump.Pretty

renderIOTerm :: PP.LayoutOptions -> Doc AnsiStyle -> IO ()
renderIOTerm layoutOpts doc = do
    supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout
    let doc' = if supportsANSI
                 then doc
                 else PP.unAnnotate doc
    PP.renderIO stdout $ PP.layoutPretty layoutOpts doc'

type Mode = Parser (IO ())

prettyOpts :: Parser PrettyOpts
prettyOpts =
    PrettyOpts
        <$> switch (short 'u' <> long "show-uniques" <> help "Show binder uniques")
        <*> switch (short 'i' <> long "show-idinfo" <> help "Show IdInfo of bindings")
        <*> switch (short 'T' <> long "show-let-types" <> help "Show type signatures for let-bound binders")
        <*> switch (short 'U' <> long "show-unfoldings" <> help "Show unfolding templates")

dumpFile :: Parser FilePath
dumpFile = argument str (metavar "DUMP FILE" <> help "CBOR dump file")