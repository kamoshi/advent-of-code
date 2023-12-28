module Misc where

import Data.List (tails)


withCoords :: [[a]] -> [((Int, Int), a)]
withCoords grid = [((r, c), a) | (r, row) <- zip [0..] grid, (c, a) <- zip [0..] row]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs $ tail xs

paired :: [a] -> [(a, a)]
paired xs = [(x, y) | (x:ys) <- tails xs, y <- ys]
