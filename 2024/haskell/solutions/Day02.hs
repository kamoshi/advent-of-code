{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Day02 (day) where

import Advent (Day, mkDay, (|>))
import Data.Text (Text)
import Data.Text qualified as Text

parse :: Text -> Either String [[Int]]
parse text =
  text
    |> Text.lines
    |> map (map (read @Int . Text.unpack) . Text.words)
    |> Right

diff :: Int -> Int -> Bool
diff a b = let sub = a - b in 1 <= sub && sub <= 3

decreasing :: [Int] -> Bool
decreasing = \case
  (a : b : rest) -> diff a b && decreasing (b : rest)
  _ -> True

increasing :: [Int] -> Bool
increasing = \case
  (a : b : rest) -> diff b a && increasing (b : rest)
  _ -> True

solveA :: [[Int]] -> Int
solveA input =
  input
    |> filter ((||) <$> decreasing <*> increasing)
    |> length

solveB :: [[Int]] -> Int
solveB input =
  input
    |> map permutate
    |> filter (any ((||) <$> decreasing <*> increasing))
    |> length
  where
    permutate = \case
      list@(h : t) -> list : t : map (h :) (permutate t)
      _ -> []

day :: Day
day = mkDay 2 parse solveA solveB
