{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.HUnit
import qualified System.Exit as Exit
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04


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
day03 =
  let parsed = Day03.parse input
  in TestList
  [ TestCase $ assertEqual "A" (Right 4361)   (Day03.solveA <$> parsed)
  , TestCase $ assertEqual "B" (Right 467835) (Day03.solveB <$> parsed)
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

day04 :: Test
day04 =
  let parsed = Day04.parse input
  in TestList
    [ TestCase $ assertEqual "A" (Right 13) (Day04.solveA <$> parsed)
    , TestCase $ assertEqual "B" (Right 30) (Day04.solveB <$> parsed)
    ]
  where
    input =
      "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
      \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
      \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
      \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
      \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
      \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n"

tests :: Test
tests = TestList
  [ TestLabel "day01" day01
  , TestLabel "day02" day02
  , TestLabel "day03" day03
  , TestLabel "day04" day04
  ]

main :: IO ()
main = isFailed . failures =<< runTestTT tests
  where
    isFailed count
      | count > 0 = Exit.exitFailure
      | otherwise = Exit.exitSuccess
