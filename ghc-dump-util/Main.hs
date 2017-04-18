{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (sortBy)
import Data.Monoid
import Data.Ord
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Control.Monad

import GhcDump.Pretty
import GhcDump.Util
import GhcDump.Ast

data Column a = Col { colWidth :: Int, colHeader :: String, colGet :: (a -> Doc) }

type Table a = [Column a]

renderTable :: forall a. Table a -> [a] -> Doc
renderTable cols xs =
         row (PP.bold . text . colHeader)
    <$$> vcat [ row (flip colGet x) | x <- xs ]
  where
    row :: (Column a -> Doc) -> Doc
    row toCell = go cols
      where
        go :: [Column a] -> Doc
        go []           = PP.empty
        go [col]        = align $ toCell col
        go (col : rest) = fillBreak (colWidth col) (align $ toCell col) PP.<+> go rest

modes :: Parser (IO ())
modes = subparser
     $ mode "show" showMode (progDesc "print Core")
    <> mode "list-bindings" listBindingsMode (progDesc "list top-level bindings, their sizes, and types")
    <> mode "summarize" summarizeMode (progDesc "summarize multiple dump files")
  where
    mode name f opts = command name (info (helper <*> f) opts)

    dumpFile :: Parser FilePath
    dumpFile = argument str (metavar "DUMP FILE" <> help "CBOR dump file")

    prettyOpts :: Parser PrettyOpts
    prettyOpts =
        PrettyOpts
          <$> switch (short 'u' <> long "show-uniques" <> help "Show binder uniques")
          <*> switch (short 'i' <> long "show-idinfo" <> help "Show IdInfo of bindings")
          <*> switch (short 'T' <> long "show-let-types" <> help "Show type signatures for let-bound binders")

    showMode =
        run <$> prettyOpts <*> dumpFile
      where
        run opts fname = do
            dump <- GhcDump.Util.readDump fname
            print $ pprModule opts dump

    listBindingsMode =
        run <$> sortField <*> prettyOpts <*> dumpFile
      where
        sortField =
            option (str >>= readSortField)
                   (long "sort" <> short 's' <> value id
                    <> help "Sort by (accepted values: terms, types, coercions, type)")
          where
            readSortField "terms"     = return $ sortBy (flip $ comparing $ csTerms . getStats)
            readSortField "types"     = return $ sortBy (flip $ comparing $ csTypes . getStats)
            readSortField "coercions" = return $ sortBy (flip $ comparing $ csCoercions . getStats)
            readSortField "type"      = return $ sortBy (comparing $ binderType . unBndr . getBinder)
            readSortField f           = fail $ "unknown sort field "++f

        run sortBindings opts fname = do
            dump <- GhcDump.Util.readDump fname
            let table = [ Col 20 "Name"   (pprBinder opts . getBinder)
                        , Col 6  "Terms"  (pretty . csTerms . getStats)
                        , Col 6  "Types"  (pretty . csTypes . getStats)
                        , Col 6  "Coerc." (pretty . csCoercions . getStats)
                        , Col 300 "Type"  (pprType opts . binderType . unBndr . getBinder)
                        ]
            print $ renderTable table (sortBindings $ moduleBindings dump)

    summarizeMode =
        run <$> some dumpFile
      where
        run fnames = do
            mods <- mapM (\fname -> do mod <- readDump fname
                                       return (fname, mod)) fnames
            let totalSize :: Module -> CoreStats
                totalSize = foldMap getStats . moduleBindings
            let table = [ Col 35 "Name" (text . fst)
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
