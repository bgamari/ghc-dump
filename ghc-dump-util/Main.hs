import Options.Applicative

import Control.Monad

import GhcDump.Pretty
import GhcDump.Util
import GhcDump.Ast

modes :: Parser (IO ())
modes = subparser $
    command "show" (info (helper <*> showMode) mempty)
  where
    dumpFile :: Parser FilePath
    dumpFile = argument str (help "CBOR dump file")

    showMode =
        run <$> dumpFile
      where
        run fname = do
            dump <- GhcDump.Util.readDump fname
            print $ pretty dump

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
