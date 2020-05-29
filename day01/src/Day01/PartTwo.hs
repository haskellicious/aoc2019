module Day01.PartTwo
  ( solve
  ) where

solve :: String -> Int
solve = sum . map (fuel 0 . read) . words

fuel :: Int -> Int -> Int
fuel total mass
  | f <= 0 = total
  | otherwise = fuel (total + f) f
  where
    f = fuel' mass

fuel' :: Int -> Int
fuel' value = (value `div` 3) - 2
