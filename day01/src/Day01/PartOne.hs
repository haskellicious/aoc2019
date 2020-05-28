module Day01.PartOne
  ( solve
  ) where

solve :: String -> Int
solve = sum . map (fuel . read) . words

fuel :: Int -> Int
fuel value = (value `div` 3) - 2
