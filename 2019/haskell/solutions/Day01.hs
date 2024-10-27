{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Day01 (day) where

import Advent (Day, mkDay, (|>))
import Data.Text (Text)
import Data.Text qualified as Text

parse :: Text -> Either String [Int]
parse text =
  text
    |> Text.lines
    |> map (read @Int . Text.unpack)
    |> Right

solveA :: [Int] -> Int
solveA = sum . map calculate
 where
  calculate n = n `div` 3 - 2

solveB :: [Int] -> Int
solveB = sum . map calculate
 where
  calculate number =
    number
      |> iterate (\n -> n `div` 3 - 2)
      |> drop 1
      |> takeWhile (> 0)
      |> foldl' (+) 0

day :: Day
day = mkDay 1 parse solveA solveB
