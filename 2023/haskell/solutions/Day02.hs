{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Day02 (Cube(..), Game(..), parse, solveA, solveB) where

import Data.Void (Void)
import Data.Text (Text)
import Data.Bifunctor (first)
import Text.Megaparsec (Parsec, optional, choice, eof, many, sepBy, runParser, errorBundlePretty)
import Text.Megaparsec.Char (string, space, char, newline)
import Text.Megaparsec.Char.Lexer (decimal)


data Cube = R Int | G Int | B Int deriving Show

data Game = Game
  { gId   :: Int
  , gSets :: [[Cube]]
  } deriving Show

type Parser = Parsec Void Text


parse :: Text -> Either String [Game]
parse = first errorBundlePretty . runParser games ""
  where
    cube :: Parser Cube
    cube = do
      _     <- optional space
      count <- decimal
      space
      color <- choice
        [ R <$ string "red"
        , G <$ string "green"
        , B <$ string "blue"
        ]
      return $ color count
    set :: Parser [Cube]
    set = space *> cube `sepBy` char ','
    sets :: Parser [[Cube]]
    sets = set `sepBy` char ';'
    game :: Parser Game
    game = do
      _     <- string "Game"
      space
      gId   <- decimal
      _     <- char ':'
      gSets <- sets
      _     <- optional newline
      return $ Game {..}
    games :: Parser [Game]
    games = many game <* eof


solveA :: [Game] -> Int
solveA = sum . map gId . filter isValid
  where
    isSetValid :: Cube -> Bool
    isSetValid count = case count of
      R r -> r <= 12
      G g -> g <= 13
      B b -> b <= 14
    isValid :: Game -> Bool
    isValid Game {..} = all (all isSetValid) gSets

solveB :: [Game] -> Int
solveB = sum . map toPower
  where
    update :: (Int, Int, Int) -> Cube -> (Int, Int, Int)
    update (r, g, b) cube = case cube of
      R r' -> (max r r', g, b)
      G g' -> (r, max g g', b)
      B b' -> (r, g, max b b')
    toPower :: Game -> Int
    toPower Game {..} = (\(r, g, b) -> r * g * b) . foldl update (0, 0, 0) . concat $ gSets
