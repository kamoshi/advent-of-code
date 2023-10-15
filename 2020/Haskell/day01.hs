module Day01 where

import Data.List (tails)
import Data.Maybe (fromMaybe)


example :: [Integer]
example =
  [ 1721
  , 979
  , 366
  , 299
  , 675
  , 1456
  ]

scout :: Int -> [[Integer]] -> Maybe [[Integer]]
scout _ []    = Nothing
scout 0 _     = Just []
scout 1 (h:_) = Just $ map (:[]) h
scout n (h:t) = do
  a <- map (head h:) <$> scout (n-1) t
  let b = fromMaybe [] $ scout n t
  pure $ a <> b


solve :: Int -> [Integer] -> Integer
solve n = product . head . filter ((2020==) . sum) . fromMaybe [] . scout n . tails

solve1 :: [Integer] -> Integer
solve1 = solve 2

solve2 :: [Integer] -> Integer
solve2 = solve 3

main :: IO ()
main = do
  contents <- readFile "../.inputs/01.txt"
  let list = map read $ words contents
  print $ solve1 list
  print $ solve2 list
