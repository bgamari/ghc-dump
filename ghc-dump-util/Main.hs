import Data.List (sortBy)
import Data.Monoid
import Data.Ord
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

import Control.Monad

import GhcDump.Pretty
import GhcDump.Util
import GhcDump.Ast

modes :: Parser (IO ())
modes = subparser
     $ mode "show" showMode
    <> mode "list-bindings" listBindingsMode
  where
    mode name f = command name (info (helper <*> f) mempty)

    dumpFile :: Parser FilePath
    dumpFile = argument str (help "CBOR dump file")

    showMode =
        run <$> dumpFile
      where
        run fname = do
            dump <- GhcDump.Util.readDump fname
            print $ pretty dump

    listBindingsMode =
        run <$> sortField <*> dumpFile
      where
        sortField =
            option (str >>= readSortField)
                   (long "sort" <> short 's' <> value id
                    <> help "Sort by (accepted values: terms, types, coercions, type)")
          where
            readSortField "terms"     = return $ sortBy (flip $ comparing $ csTerms . getStats)
            readSortField "types"     = return $ sortBy (flip $ comparing $ csTypes . getStats)
            readSortField "coercions" = return $ sortBy (flip $ comparing $ csCoercions . getStats)
            --readSortField "type"      = return $ sortBy (comparing $ binderType . unBndr . getBinder)
            readSortField f           = fail $ "unknown sort field "++f

            getBinder (b,_,_) = b
            getStats (_,s,_) = s

        run sortBindings fname = do
            dump <- GhcDump.Util.readDump fname
            let rows = vcat $ map bindingRow $ sortBindings $ moduleBindings dump
                bindingRow (b, s, _) =
                    row (pretty b) (pretty $ csTerms s) (pretty $ csTypes s)
                        (pretty $ csCoercions s) (binderType $ unBndr b)
                header = row (text "Name") (text "Terms") (text "Types") (text "Coer.") (text "Type")
                row name terms types coercions ty =
                    fillBreak 20 name
                    <+> fillBreak 6 terms
                    <+> fillBreak 6 types
                    <+> fillBreak 6 coercions
                    <+> pretty ty
            print (header <$$> rows)

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
