{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.HUnit (Test(..), assertEqual, runTestTT, failures)
import qualified System.Exit as Exit
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05


day01 :: Test
day01 =
  let parsedA = Day01.parse inputA
      parsedB = Day01.parse inputB
  in TestList
    [ TestCase $ assertEqual "A" (Right 142) (Day01.solveA <$> parsedA)
    , TestCase $ assertEqual "B" (Right 281) (Day01.solveB <$> parsedB)
    ]
  where
    inputA =
      "1abc2\n\
      \pqr3stu8vwx\n\
      \a1b2c3d4e5f\n\
      \treb7uchet\n"
    inputB =
      "two1nine\n\
      \eightwothree\n\
      \abcone2threexyz\n\
      \xtwone3four\n\
      \4nineeightseven2\n\
      \zoneight234\n\
      \7pqrstsixteen\n"

day02 :: Test
day02 =
  let parsed = Day02.parse input
  in TestList
    [ TestCase $ assertEqual "A" (Right 8)    (Day02.solveA <$> parsed)
    , TestCase $ assertEqual "B" (Right 2286) (Day02.solveB <$> parsed)
    ]
  where
    input =
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
      \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
      \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
      \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
      \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"

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

day05 :: Test
day05 =
  let parsed = Day05.parse input
  in TestList
    [ TestCase $ assertEqual "A" (Right 35) (Day05.solveA <$> parsed)
    , TestCase $ assertEqual "B" (Right 46) (Day05.solveB <$> parsed)
    ]
  where
    input =
      "seeds: 79 14 55 13\n\
      \\n\
      \seed-to-soil map:\n\
      \50 98 2\n\
      \52 50 48\n\
      \\n\
      \soil-to-fertilizer map:\n\
      \0 15 37\n\
      \37 52 2\n\
      \39 0 15\n\
      \\n\
      \fertilizer-to-water map:\n\
      \49 53 8\n\
      \0 11 42\n\
      \42 0 7\n\
      \57 7 4\n\
      \\n\
      \water-to-light map:\n\
      \88 18 7\n\
      \18 25 70\n\
      \\n\
      \light-to-temperature map:\n\
      \45 77 23\n\
      \81 45 19\n\
      \68 64 13\n\
      \\n\
      \temperature-to-humidity map:\n\
      \0 69 1\n\
      \1 0 69\n\
      \\n\
      \humidity-to-location map:\n\
      \60 56 37\n\
      \56 93 4\n"

tests :: Test
tests = TestList
  [ TestLabel "day01" day01
  , TestLabel "day02" day02
  , TestLabel "day03" day03
  , TestLabel "day04" day04
  , TestLabel "day05" day05
  ]

main :: IO ()
main = isFailed . failures =<< runTestTT tests
  where
    isFailed count
      | count > 0 = Exit.exitFailure
      | otherwise = Exit.exitSuccess
