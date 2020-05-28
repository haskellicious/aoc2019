module Day02.PartOne
  ( solve
  ) where

import           Data.List.Split

solve :: String -> Int
solve = head . process 0 . program
  where
    program = setup . map read . splitOn ","
    setup (x:_:_:xs) = x : [12, 2] ++ xs

process :: Int -> [Int] -> [Int]
process pc program
  | code == 1 = next $ exec (+)
  | code == 2 = next $ exec (*)
  | code == 99 = program
  where
    code = program !! pc
    next = process $ pc + 4
    exec op = set program result $ op (value program 1) (value program 2)
      where
        result = (program !! (pc + 3))
        value p o = (p !! (p !! (pc + o)))
        set l i v = (take i l) ++ v : (snd $ splitAt (i + 1) l)
