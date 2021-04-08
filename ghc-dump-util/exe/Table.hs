{-# LANGUAGE ScopedTypeVariables #-}

module Table where

import Prettyprinter as PP
import Prettyprinter.Render.Terminal as PP
import qualified System.Console.ANSI

import GhcDump.Pretty

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
