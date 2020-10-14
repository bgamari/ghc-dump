{-# LANGUAGE BangPatterns #-}

module Cpr where

f :: Int -> (Int, Int)
f !n = (n+1, n+2)

-- https://gitlab.haskell.org/ghc/ghc/-/wikis/nested-cpr/split-off-cpr
