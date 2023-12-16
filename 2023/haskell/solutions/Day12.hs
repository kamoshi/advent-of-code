{-# LANGUAGE OverloadedStrings #-}
module Day12 where

import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, many, eof, choice, sepBy)
import Data.Bifunctor (first)
import Text.Megaparsec.Char (char, space, newline)
import Text.Megaparsec.Char.Lexer (decimal)


data Cell
  = U -- ? unknown
  | O -- . operational
  | D -- # damaged
  deriving Show

type Row = ([Cell], [Int])

type Parser = Parsec Void Text


input :: Text
input =
  "???.### 1,1,3\n\
  \.??..??...?##. 1,1,3\n\
  \?#?#?#?#?#?#?#? 1,3,1,6\n\
  \????.#...#... 4,1,1\n\
  \????.######..#####. 1,6,5\n\
  \?###???????? 3,2,1\n"

parse :: Text -> Either String [Row]
parse = first errorBundlePretty . runParser rows ""
  where
    cell :: Parser Cell
    cell = choice [U <$ char '?', O <$ char '.', D <$ char '#']
    row :: Parser Row
    row = do
      cs <- many cell <* space
      ns <- decimal `sepBy` char ',' <* newline
      return (cs, ns)
    rows :: Parser [Row]
    rows = many row <* eof

arrange :: [Cell] -> [Int] -> [[Cell]]
arrange = _

solveA :: [Row] -> Int
solveA = length . concatMap (uncurry arrange)

-- >>> parse input
-- Right [([U,U,U,O,D,D,D],[1,1,3]),([O,U,U,O,O,U,U,O,O,O,U,D,D,O],[1,1,3]),([U,D,U,D,U,D,U,D,U,D,U,D,U,D,U],[1,3,1,6]),([U,U,U,U,O,D,O,O,O,D,O,O,O],[4,1,1]),([U,U,U,U,O,D,D,D,D,D,D,O,O,D,D,D,D,D,O],[1,6,5]),([U,D,D,D,U,U,U,U,U,U,U,U],[3,2,1])]

