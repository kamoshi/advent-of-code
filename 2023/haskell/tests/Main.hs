{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.HUnit
import qualified System.Exit as Exit
import qualified Day01


day01 :: Test
day01 = TestList
  [ TestCase $ assertEqual "should return 142" 142 (Day01.solveA inputA)
  , TestCase $ assertEqual "should return 281" 281 (Day01.solveB inputB)
  ]
  where
    inputA = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]
    inputB = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]


tests :: Test
tests = TestList
  [ TestLabel "day01" day01
  ]

main :: IO ()
main = isFailed . failures =<< runTestTT tests
  where
    isFailed count
      | count > 0 = Exit.exitFailure
      | otherwise = Exit.exitSuccess
