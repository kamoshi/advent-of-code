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
  deriving (Show, Eq)

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
arrange [] [] = [[]]
arrange [] _  = []
arrange cs []
  | D `notElem` cs = [map (const O) cs]
  | otherwise      = []
arrange cs@(c:cr) ns@(n:nr)
  | canFill && canSkip = tryFill <> trySkip
  | canFill            = tryFill
  | canSkip            = trySkip
  | otherwise          = []
  where
    (window, rest) = splitAt n cs
    fill :: [Cell] -> [Cell]
    fill = (replicate n D ++)
    canSkip :: Bool
    canSkip = c /= D
    trySkip :: [[Cell]]
    trySkip = map (O:) (arrange cr ns)
    canFill :: Bool
    canFill = O `notElem` window && n == length window && (null rest || head rest /= D)
    tryFill :: [[Cell]]
    tryFill
      | null rest = map fill (arrange rest nr)
      | otherwise = map (fill . (O:)) (arrange (tail rest) nr)

solveA :: [Row] -> Int
solveA = length . concatMap (uncurry arrange)

-- >>> solveA <$> parse input
-- Right 21

