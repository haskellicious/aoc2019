module Day02.PartOne
  ( solve
  ) where

import           Data.List.Split

solve :: String -> Int
solve = head . process 0 . program
  where
    program = init . map read . splitOn ","
    init (x:_:_:xs) = x : [12, 2] ++ xs

process :: Int -> [Int] -> [Int]
process pc prg
  | code == 1 = next $ sum
  | code == 2 = next $ mul
  | code == 99 = prg
  where
    code = prg !! pc
    next = process $ pc + 4
    sum = set prg (prg !! (pc + 3)) $ (value prg 1) + (value prg 2)
    mul = set prg (prg !! (pc + 3)) $ (value prg 1) * (value prg 2)
    value p o = (p !! (p !! (pc + o)))
    set l i v = (take i l) ++ (v : (snd $ splitAt (i + 1) l))
