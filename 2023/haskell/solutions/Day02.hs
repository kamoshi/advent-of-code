{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Day02 (Cube(..), Game(..), parse, solveA) where

import Data.Void (Void)
import Data.Text (Text)
import Data.Bifunctor (first)
import Text.Megaparsec (Parsec, optional, choice, eof, many, sepBy, runParser, errorBundlePretty)
import Text.Megaparsec.Char (string, space, char, newline)
import Text.Megaparsec.Char.Lexer (decimal)


data Cube = R Int | G Int | B Int deriving Show

data Game = Game
  { gameId   :: Int
  , gameSets :: [[Cube]]
  } deriving Show

type Parser = Parsec Void Text


parse :: Text -> Either String [Game]
parse = first errorBundlePretty . runParser games ""
  where
    cube :: Parser Cube
    cube = do
      _ <- optional space
      number <- decimal
      space
      color <- choice
        [ R <$ string "red"
        , G <$ string "green"
        , B <$ string "blue"
        ]
      return . color $ number
    set :: Parser [Cube]
    set = space *> cube `sepBy` char ','
    sets :: Parser [[Cube]]
    sets = set `sepBy` char ';'
    game :: Parser Game
    game = do
      _ <- string "Game"
      space
      gameId <- decimal
      _ <- char ':'
      gameSets <- sets
      _ <- optional newline
      return $ Game {..}
    games :: Parser [Game]
    games = many game <* eof


solveA :: [Game] -> Int
solveA = sum . map gameId . filter isValid
  where
    isSetValid :: Cube -> Bool
    isSetValid count = case count of
      R r -> r <= 12
      G g -> g <= 13
      B b -> b <= 14
    isValid :: Game -> Bool
    isValid Game {..} = all (all isSetValid) gameSets
