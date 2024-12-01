{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Day01 (day) where

import Advent (Day, mkDay, (|>))
import Data.Bifunctor (bimap)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (traceShowId)

sample :: Text
sample =
  "3   4\n\
  \4   3\n\
  \2   5\n\
  \1   3\n\
  \3   9\n\
  \3   3"

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

-- $> fmap (solveA) (parse sample)

solveA input =
  input
    |> bimap List.sort List.sort
    |> uncurry List.zip
    |> List.map (abs . uncurry (-))
    |> sum

solveB = const 2

day :: Day
day = mkDay 1 parse solveA solveB
