module Day01.PartTwo
  ( solve
  ) where

import qualified Day01.PartOne

solve :: String -> Int
solve = sum . map (fuel 0 . read) . words

fuel :: Int -> Int -> Int
fuel total mass
  | f <= 0 = total
  | otherwise = fuel (total + f) f
  where
    f = Day01.PartOne.fuel mass
