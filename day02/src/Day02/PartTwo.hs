module Day02.PartTwo
  ( solve
  ) where

import           Data.List.Split
import qualified Day02.PartOne

solve :: String -> Int
solve = process 0 0 . program
  where
    program = map read . splitOn ","

process :: Int -> Int -> [Int] -> Int
process noun verb program
  | output == 19690720 = 100 * noun + verb
  | noun == 99 && verb == 99 = 0
  | verb == 99 = process (noun + 1) 0 program
  | otherwise = process noun (verb + 1) program
  where
    output = head $ Day02.PartOne.process 0 $ setup program
    setup (x:_:_:xs) = x : [noun, verb] ++ xs
