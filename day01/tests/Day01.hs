module Main
  ( main
  ) where

import           Day01.PartOne
import           Day01.PartTwo
import           System.Environment
import           System.Exit
import           Test.HUnit
import           Test.HUnit.Base

run = flip fmap $ readFile "input.txt"

partOne :: Test
partOne =
  TestCase $ do
    answer <- run Day01.PartOne.solve
    assertEqual "the right answer is 3364035" 3364035 (read answer)

partTwo :: Test
partTwo =
  TestCase $ do
    answer <- run Day01.PartTwo.solve
    assertEqual "the right answer is 5043167" 5043167 (read answer)

main = do
  counts <- runTestTT $ TestList [partOne, partTwo]
  finish $ (errors counts) + (failures counts)
  where
    finish 0 = exitSuccess
    finish _ = exitFailure
