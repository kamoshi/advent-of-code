module Day03 (parse, solve1, solve2) where

import Data.List (nub, partition)


data Move = L | R | U | D deriving Show

parse :: String -> [Move]
parse = map mapper
  where
    mapper c = case c of
      '<' -> L
      '>' -> R
      '^' -> U
      'v' -> D

findPath :: [Move] -> [(Int, Int)]
findPath = scanl move (0, 0)
  where
    move (x, y) dir = case dir of
      L -> (x-1, y)
      R -> (x+1, y)
      U -> (x, y+1)
      D -> (x, y-1)

solve1 :: [Move] -> Int
solve1 = length . nub . findPath

solve2 :: [Move] -> Int
solve2 moves =
  let (xs, ys) = partition (even . fst) $ zip [0..] moves
  in length . nub $ findPath' xs <> findPath' ys
    where
      findPath' = findPath . map snd
