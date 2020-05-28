module Day01.PartTwo
  ( solve
  ) where

solve :: String -> String
solve = show . sum . map ((fuel 0) . read) . words

fuel :: Integer -> Integer -> Integer
fuel total mass
  | f <= 0 = total
  | otherwise = fuel (total + f) $ f
  where
    f = fuel' mass

fuel' :: Integer -> Integer
fuel' value = (value `div` 3) - 2
