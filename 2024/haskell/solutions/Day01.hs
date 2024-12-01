{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Day01 (day) where

import Advent (Day, mkDay, (|>))
import Data.Bifunctor (bimap)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

parse :: Text -> Either String ([Int], [Int])
parse text =
  text
    |> Text.lines
    |> List.map (List.map (read @Int . Text.unpack) . Text.splitOn "   ")
    |> List.transpose
    |> parsePair
  where
    parsePair = \case
      [as, bs] -> Right (as, bs)
      _ -> Left "Malformed input"

solveA :: ([Int], [Int]) -> Int
solveA input =
  input
    |> bimap List.sort List.sort
    |> uncurry List.zip
    |> List.map (abs . uncurry (-))
    |> sum

count :: [Int] -> IntMap Int
count = foldr inc IntMap.empty
  where
    inc n = IntMap.insertWith (+) n 1

solveB :: ([Int], [Int]) -> Int
solveB (as, bs) =
  as
    |> List.map (\n -> n * get n)
    |> sum
  where
    get key =
      count bs
        |> IntMap.lookup key
        |> fromMaybe 0

day :: Day
day = mkDay 1 parse solveA solveB
