module Day01 (day) where

import Advent (Day, mkDay)
import Data.Text (Text)

parse :: Text -> Either String Int
parse _ = Right 33

-- $> print . parse $ "33"

solveA :: Int -> Int
solveA = (+ 1)

solveB :: Int -> Int
solveB = (* 2)

day :: Day
day = mkDay 1 parse solveA solveB
