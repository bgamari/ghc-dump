{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Maybe
import Data.List (sortBy)
import Data.Ord
import System.IO (stdout)

import Options.Applicative
import Prettyprinter as PP
import Prettyprinter.Render.Terminal as PP
import qualified System.Console.ANSI

import Text.Regex.TDFA
import Text.Regex.TDFA.Common (Regex)
import Text.Regex.TDFA.Text ()

import GhcDump.ToHtml
import GhcDump.Pretty
import GhcDump.Util
import GhcDump.Ast

data Column a = Col { colWidth :: Int
                    , colHeader :: String
                    , colGet :: a -> Doc AnsiStyle
                    }

type Table a = [Column a]

renderTable :: forall a. Table a -> [a] -> Doc AnsiStyle
renderTable cols xs =
         row (PP.annotate PP.bold . pretty . colHeader)
    <$$> vcat [ row (flip colGet x) | x <- xs ]
  where
    row :: (Column a -> Doc AnsiStyle) -> Doc AnsiStyle
    row toCell = go cols
      where
        go :: [Column a] -> Doc AnsiStyle
        go []           = mempty
        go [col]        = align $ toCell col
        go (col : rest) = fillBreak (colWidth col) (align $ toCell col) PP.<+> go rest

filterBindings :: Regex -> Module -> Module
filterBindings re m =
    m { moduleTopBindings = mapMaybe filterTopBinding $ moduleTopBindings m }
  where
    filterTopBinding b'@(NonRecTopBinding b _ _)
      | nameMatches b  = Just b'
      | otherwise      = Nothing
    filterTopBinding (RecTopBinding bs)
      | not $ null bs' = Just $ RecTopBinding bs'
      | otherwise      = Nothing
      where bs' = filter (\(b,_,_) -> nameMatches b) bs

    nameMatches :: Binder -> Bool
    nameMatches b = matchTest re (binderUniqueName b)

renderIOTerm :: PP.LayoutOptions -> Doc AnsiStyle -> IO ()
renderIOTerm layoutOpts doc = do
    supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout
    let doc' = if supportsANSI
                 then doc
                 else PP.unAnnotate doc
    PP.renderIO stdout $ PP.layoutPretty layoutOpts doc'

modes :: Parser (IO ())
modes = subparser
     $ mode "show" showMode (progDesc "print Core")
    <> mode "list-bindings" listBindingsMode (progDesc "list top-level bindings, their sizes, and types")
    <> mode "summarize" summarizeMode (progDesc "summarize multiple dump files")
  where
    mode name f opts = command name (info (helper <*> f) opts)

    dumpFile :: Parser FilePath
    dumpFile = argument str (metavar "DUMP FILE" <> help "CBOR dump file")

    filterCond :: Parser (Module -> Module)
    filterCond =
        fmap (maybe id filterBindings)
        $ option (str >>= fmap Just . makeRegexM')
                 (short 'f' <> long "filter" <> value Nothing <> help "filter bindings by name")
      where
        makeRegexM' = makeRegexM :: String -> ReadM Regex

    bindingsSort :: Parser (Module -> Module)
    bindingsSort =
        option (str >>= readSortField)
               (long "sort" <> short 's' <> value id
                <> help "Sort by (accepted values: none, terms, types, coercions, type)")
      where
        readSortField "none"      = return $ id
        readSortField "terms"     = return $ onBinds $ sortBy (flip $ comparing $ csTerms . getStats)
        readSortField "types"     = return $ onBinds $ sortBy (flip $ comparing $ csTypes . getStats)
        readSortField "coercions" = return $ onBinds $ sortBy (flip $ comparing $ csCoercions . getStats)
        readSortField "type"      = return $ onBinds $ sortBy (comparing $ binderType . unBndr . getBinder)
        readSortField f           = fail $ "unknown sort field "++f

        onBinds :: ([(Binder, CoreStats, Expr)] -> [(Binder, CoreStats, Expr)])
                -> Module -> Module
        onBinds f mod = mod { moduleTopBindings = [RecTopBinding $ f $ moduleBindings mod] }

    prettyOpts :: Parser PrettyOpts
    prettyOpts =
        PrettyOpts
          <$> switch (short 'u' <> long "show-uniques" <> help "Show binder uniques")
          <*> switch (short 'i' <> long "show-idinfo" <> help "Show IdInfo of bindings")
          <*> switch (short 'T' <> long "show-let-types" <> help "Show type signatures for let-bound binders")
          <*> switch (short 'U' <> long "show-unfoldings" <> help "Show unfolding templates")

    showMode =
        run <$> filterCond <*> bindingsSort <*> prettyOpts <*> html <*> dumpFile
      where
        html = switch (short 'H' <> long "html" <> help "Render to HTML")
        run filterFn sortBindings opts html fname = do
            dump <- sortBindings . filterFn <$> GhcDump.Util.readDump fname
            if html
              then writeFile "out.html" $ show $ topBindingsToHtml (moduleTopBindings dump)
              else renderIOTerm PP.defaultLayoutOptions $ pprModule opts dump

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

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
