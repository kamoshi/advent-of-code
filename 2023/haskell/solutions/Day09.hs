{-# LANGUAGE OverloadedStrings #-}
module Day09 (parse, solveA, solveB) where

import Data.Void (Void)
import Data.Text (Text)
import Data.Bifunctor (first)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, sepBy, many, eof)
import Text.Megaparsec.Char (char, newline, space)
import Text.Megaparsec.Char.Lexer (signed, decimal)


type Parser = Parsec Void Text


parse :: Text -> Either String [[Int]]
parse = first errorBundlePretty . runParser nss ""
  where
    nss :: Parser [[Int]]
    nss = many (signed space decimal `sepBy` char ' ' <* newline) <* eof

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs $ tail xs

diffs :: [Int] -> [[Int]]
diffs ns
  | all (0 ==) ns = [ns]
  | otherwise     = ns : diffs (diff ns)
  where
    diff :: [Int] -> [Int]
    diff = map (uncurry $ flip (-)) . pairs

solve :: ([Int] -> Int -> Int) -> [[Int]] ->  Int
solve op = sum . map (foldr op 0 . diffs)

solveA :: [[Int]] -> Int
solveA = solve $ (+) . last

solveB :: [[Int]] -> Int
solveB = solve $ (-) . head
