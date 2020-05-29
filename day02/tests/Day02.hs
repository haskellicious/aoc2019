module Main
  ( main
  ) where

import qualified Day02.PartOne
import qualified Day02.PartTwo
import           System.Environment
import           System.Exit
import           Test.HUnit

run = flip fmap $ readFile "input.txt"

partOne =
  TestCase $ do
    answer <- run Day02.PartOne.solve
    assertEqual "the right answer is 3058646" 3058646 answer

partTwo =
  TestCase $ do
    answer <- run Day02.PartTwo.solve
    assertEqual "the right answer is 8976" 8976 answer

main = do
  counts <- runTestTT $ TestList [partOne, partTwo]
  finish $ errors counts + failures counts
  where
    finish 0 = exitSuccess
    finish _ = exitFailure
