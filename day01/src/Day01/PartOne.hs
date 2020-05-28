module Day01.PartOne
  ( solve
  ) where

solve :: String -> String
solve = show . sum . map (fuel . read) . words

fuel :: Integer -> Integer
fuel value = (value `div` 3) - 2
