{-# LANGUAGE OverloadedStrings #-}
module Day03 (parse, solveA, solveB) where

import Data.Bifunctor (first)
import Data.Void (Void)
import Data.Char (digitToInt, isSpace)
import Data.Text (Text)
import Text.Megaparsec (errorBundlePretty, Parsec, runParser, many, eof, choice, satisfy, manyTill)
import Text.Megaparsec.Char (digitChar, char)


data Item
  = Digit Int
  | Symbol Char
  | Empty
  deriving (Show, Eq)

type Parser = Parsec Void Text


parse :: Text -> Either String [[Item]]
parse = first errorBundlePretty . runParser full ""
  where
    item :: Parser Item
    item = choice
      [ Digit . digitToInt <$> digitChar
      , Empty  <$  char '.'
      , Symbol <$> satisfy (not . isSpace)
      ]
    line :: Parser [Item]
    line = item `manyTill` satisfy isSpace
    full :: Parser [[Item]]
    full = many line <* eof

solveA :: [[Item]] -> Int
solveA = const 1

solveB :: [[Item]] -> Int
solveB = const 2

-- >>> parse input
-- Right [Digit 4,Digit 6,Digit 7,Empty,Empty,Digit 1,Digit 1,Digit 4,Empty,Empty]

