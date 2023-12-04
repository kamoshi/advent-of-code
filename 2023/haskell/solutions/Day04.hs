{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Day04 (parse, solveA, solveB) where

import Data.Void (Void)
import Data.Text (Text)
import Data.Bits (shiftL)
import Data.Bifunctor (first)
import Text.Megaparsec (Parsec, many, eof, runParser, errorBundlePretty)
import Text.Megaparsec.Char (string, space, char)
import Text.Megaparsec.Char.Lexer (decimal)


data Card = Card
  { cId   :: Int
  , cWins :: [Int]
  , cNums :: [Int]
  } deriving Show

type Parser = Parsec Void Text


parse :: Text -> Either String [Card]
parse = first errorBundlePretty . runParser cards ""
  where
    nums :: Parser [Int]
    nums = many $ decimal <* space
    card :: Parser Card
    card = do
      _      <- string "Card"
      space
      cId    <- decimal
      _      <- char ':'
      space
      cWins  <- nums
      _      <- char '|'
      space
      cNums  <- nums
      return $ Card {..}
    cards :: Parser [Card]
    cards = many card <* eof

intersect :: [Int] -> [Int] -> [Int]
intersect ns = filter (`elem` ns)

matching :: Card -> [Int]
matching Card {..} = cWins `intersect` cNums

toScore :: [Int] -> Int
toScore [] = 0
toScore ns = (1 `shiftL`) . pred . length $ ns

solveA :: [Card] -> Int
solveA = sum . map (toScore . matching)

copies :: Card -> [Int]
copies c@Card {..} = map (cId +) [1 .. length . matching $ c]

solveB :: [Card] -> Int
solveB cs = length . fst . head . filter (null . snd) . iterate nextState $ ([], cs)
  where
    nextState :: ([Int], [Card]) -> ([Int], [Card])
    nextState (acc, cards) =
      let acc'   = map cId cards <> acc
          cards' = map ((cs !!) . pred) $ concatMap copies cards
      in (acc', cards')
