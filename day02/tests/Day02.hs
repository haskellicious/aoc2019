module Main
  ( main
  ) where

import           Day02.PartOne
import           System.Environment
import           System.Exit
import           Test.HUnit
import           Test.HUnit.Base

run = flip fmap $ readFile "input.txt"

partOne :: Test
partOne =
  TestCase $ do
    answer <- run Day02.PartOne.solve
    assertEqual "the right answer is 3058646" 3058646 answer

main = do
  counts <- runTestTT $ TestList [partOne]
  finish $ (errors counts) + (failures counts)
  where
    finish 0 = exitSuccess
    finish _ = exitFailure
