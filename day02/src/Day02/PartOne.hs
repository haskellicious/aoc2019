module Day02.PartOne
  ( solve
  , process
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
    exec op = set result $ op (value 1) (value 2)
      where
        result = program !! (pc + 3)
        value i = program !! (program !! (pc + i))
        set i v = take i program ++ v : snd (splitAt (i + 1) program)
