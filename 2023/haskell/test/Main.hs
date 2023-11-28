module Main (main) where

import Test.HUnit
import qualified System.Exit as Exit
import qualified Day02


day01 :: Test
day01 = TestCase $ assertEqual "should return 1" 1 (Day02.xyz 0)

day02 :: Test
day02 = TestCase $ assertEqual "should return 0" 0 (Day02.xyz 0)

tests :: Test
tests = TestList
  [ TestLabel "day 1" day01
  , TestLabel "assda" day02
  ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

