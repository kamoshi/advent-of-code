{-# LANGUAGE OverloadedStrings #-}
module Day11 (parse, solveA) where

import Data.Void (Void)
import Data.Text (Text)
import Data.List (transpose, tails)
import Data.Bifunctor (first, bimap)
import Control.Monad (join)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, many, choice, eof)
import Text.Megaparsec.Char (char, newline)
import Misc (withCoords)


data Cell = E | G deriving (Show, Eq)

type Row  = Int
type Col  = Int
type Grid = [[Cell]]

type Parser = Parsec Void Text


parse :: Text -> Either String Grid
parse = first errorBundlePretty . runParser grid ""
  where
    row :: Parser [Cell]
    row = many (choice [E <$ char '.', G <$ char '#']) <* newline
    grid :: Parser Grid
    grid = many row <* eof

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

input :: Text
input =
  "...#......\n\
  \.......#..\n\
  \#.........\n\
  \..........\n\
  \......#...\n\
  \.#........\n\
  \.........#\n\
  \..........\n\
  \.......#..\n\
  \#...#.....\n"

expanded :: Grid -> ([Row], [Col])
expanded = bimap filterE (filterE . transpose) . join (,)
  where
    filterE :: Grid -> [Int]
    filterE = map fst . filter (all (E ==) . snd) . zip [0..]

galaxies :: Grid -> [(Row, Col)]
galaxies = map fst . filter ((G ==) . snd) . withCoords

solveA :: Grid -> Int
solveA = do
  gs <- galaxies
  es <- expanded
  return $ sum . map (distance es) . pairs $ gs
  where
    between :: Int -> Int -> Int -> Bool
    between a b n
      | a < b     = a <= n && n <= b
      | otherwise = b <= n && n <= a
    crossed :: Int -> Int -> [Int] -> Int
    crossed a b = length . filter (between a b)
    distance :: ([Row], [Col]) -> ((Row, Col), (Row, Col)) -> Int
    distance (rs, cs) ((r1, c1), (r2, c2)) = abs (r1 - r2) + abs (c1 - c2) + crossed r1 r2 rs + crossed c1 c2 cs

-- >>> solveA <$> parse input
-- Right 374

