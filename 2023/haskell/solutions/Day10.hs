{-# LANGUAGE OverloadedStrings #-}
module Day10 where

import Data.Void (Void)
import Data.Text (Text)
import Data.Char (isSpace)
import Data.Bifunctor (first)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, many, eof, satisfy)
import Text.Megaparsec.Char (newline)


data Pipe = E | S | UD | LR | UL | UR | DL | DR
  deriving Show

data Dir = L | R | U | D

type Row = Int
type Col = Int

type Parser = Parsec Void Text


input :: Text
input =
  ".....\n\
  \.S-7.\n\
  \.|.|.\n\
  \.L-J.\n\
  \.....\n"

parse :: Text -> Either String [[Pipe]]
parse = first errorBundlePretty . runParser grid ""
  where
    pipe :: Char -> Pipe
    pipe c = case c of
      '.' -> E
      'S' -> S
      '|' -> UD
      '-' -> LR
      'J' -> UL
      'L' -> UR
      '7' -> DL
      'F' -> DR
      bad -> error $ "Invalid character: " <> [bad]
    row :: Parser [Pipe]
    row = map pipe <$> many (satisfy $ not . isSpace) <* newline
    grid :: Parser [[Pipe]]
    grid = many row <* eof

-- >>> parse input
-- Right [[E,E,E,E,E],[E,S,LR,DL,E],[E,UD,E,UD,E],[E,UR,LR,UL,E],[E,E,E,E,E]]

