{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sample (day) where

import Advent (Day, mkDay, (|>))
import Data.Text (Text)

parse :: Text -> Either String Int
parse =
  Right 1
    |> const

-- \ $> fmap (solveB) (parse "2")

solveA :: Int -> Int
solveA = (+ 2)

solveB :: Int -> Int
solveB = (* 2)

day :: Day
day = mkDay 1 parse solveA solveB
