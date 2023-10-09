module Day01 (parse, solve1, solve2) where

import Data.Char (isPrint)
import Data.List (scanl, findIndex)
import Data.Maybe (fromJust)


parse :: String -> String
parse = filter isPrint

mapper :: Char -> Int
mapper c = case c of
  '(' -> 1
  ')' -> -1
  _ -> undefined

solve1 :: String -> Int
solve1 = sum . map mapper

solve2 :: String -> Int
solve2 = fromJust . findIndex (<0) . scanl reducer 0
  where
    reducer acc next = acc + mapper next

