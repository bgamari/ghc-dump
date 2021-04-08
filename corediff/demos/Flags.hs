module Flags where

data X = X Int Int
data Options = Options !X [Int]

flags :: Options -> [Int]
flags (Options _ xs) = xs
