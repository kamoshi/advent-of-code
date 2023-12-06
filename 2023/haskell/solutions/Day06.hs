{-# LANGUAGE OverloadedStrings #-}
module Day06 (parse, solveA, solveB) where

import Data.Void (Void)
import Data.Text (Text)
import Data.Bifunctor (first, bimap)
import Control.Monad (join)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, many, eof)
import Text.Megaparsec.Char (string, space)
import Text.Megaparsec.Char.Lexer (decimal)


type Parser = Parsec Void Text


parse :: Text -> Either String [(Int, Int)]
parse = first errorBundlePretty . runParser races ""
  where
    numbers :: Text -> Parser [Int]
    numbers s = string s *> space *> many (decimal <* space)
    races :: Parser [(Int, Int)]
    races = zip <$> numbers "Time:" <*> numbers "Distance:" <* eof

travelled :: Int -> Int -> Int
travelled t held = (t - held) * held

choices :: (Int, Int) -> [Int]
choices (t, d) = filter (d <) . map (travelled t) $ [1 .. t]

solveA :: [(Int, Int)] -> Int
solveA = product . map (length . choices)

solveB :: [(Int, Int)] -> Int
solveB = solveA . pure . join bimap (read . concatMap show) . unzip
