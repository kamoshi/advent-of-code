{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.HUnit
import qualified System.Exit as Exit
import qualified Day01
import qualified Day02
import qualified Day03


day01 :: Test
day01 = TestList
  [ TestCase $ assertEqual "A" 142 (Day01.solveA inputA)
  , TestCase $ assertEqual "B" 281 (Day01.solveB inputB)
  ]
  where
    inputA = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]
    inputB = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]

day02 :: Test
day02 = TestList
  [ TestCase $ assertEqual "A" 8    (Day02.solveA input)
  , TestCase $ assertEqual "B" 2286 (Day02.solveB input)
  ]
  where
    input =
      [ Day02.Game 1 [[Day02.B 3, Day02.R 4], [Day02.R 1, Day02.G 2, Day02.B 6], [Day02.G 2]]
      , Day02.Game 2 [[Day02.B 1, Day02.G 2], [Day02.G 3, Day02.B 4, Day02.R 1], [Day02.G 1, Day02.B 1]]
      , Day02.Game 3 [[Day02.G 8, Day02.B 6, Day02.R 20], [Day02.B 5, Day02.R 4, Day02.G 13], [Day02.G 5, Day02.R 1]]
      , Day02.Game 4 [[Day02.G 1, Day02.R 3, Day02.B 6], [Day02.G 3, Day02.R 6], [Day02.G 3, Day02.B 15, Day02.R 14]]
      , Day02.Game 5 [[Day02.R 6, Day02.B 1, Day02.G 3], [Day02.B 2, Day02.R 1, Day02.G 2]]
      ]

day03 :: Test
day03 = TestList
  [ TestCase $ assertEqual "A" (Right 4361) (Day03.solveA <$> Day03.parse input)
  ]
  where
    input =
      "467..114..\n\
      \...*......\n\
      \..35..633.\n\
      \......#...\n\
      \617*......\n\
      \.....+.58.\n\
      \..592.....\n\
      \......755.\n\
      \...$.*....\n\
      \.664.598..\n"

tests :: Test
tests = TestList
  [ TestLabel "day01" day01
  , TestLabel "day02" day02
  , TestLabel "day03" day03
  ]

main :: IO ()
main = isFailed . failures =<< runTestTT tests
  where
    isFailed count
      | count > 0 = Exit.exitFailure
      | otherwise = Exit.exitSuccess
