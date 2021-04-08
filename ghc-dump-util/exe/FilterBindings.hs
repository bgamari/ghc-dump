module FilterBindings
    ( bindingsSort
    , filterCond
    ) where

import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.List (sortBy)

import Options.Applicative
import Text.Regex.TDFA
import Text.Regex.TDFA.Common (Regex)
import Text.Regex.TDFA.Text ()

import GhcDump.Ast

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

getBinder (b,_,_) = b
getStats (_,s,_) = s
getRHS (_,_,e) = e