{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day03 (day) where

import Advent (Day, exec, mkDay)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, atEnd, optional, try, (<|>))
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

data Command = Pair Int Int | Take | Skip
  deriving (Show)

parse :: Text -> Either String [Command]
parse = exec parseData
  where
    parsePair :: Parser Command
    parsePair = do
      a <- string "mul(" *> decimal
      b <- string "," *> decimal <* string ")"
      pure $ Pair a b
    parseTake :: Parser Command
    parseTake = pure Take <* string "do()"
    parseSkip :: Parser Command
    parseSkip = pure Skip <* string "don't()"
    parseData :: Parser [Command]
    parseData = do
      done <- atEnd
      if done
        then pure []
        else do
          command <- optional (try parsePair <|> try parseTake <|> try parseSkip)
          case command of
            Just it -> (it :) <$> parseData
            Nothing -> anySingle *> parseData

solveA :: [Command] -> Int
solveA = sum . map (uncurry (*)) . mapMaybe toPair
  where
    toPair = \case
      Pair a b -> Just (a, b)
      _ -> Nothing

solveB :: [Command] -> Int
solveB = sum . map (uncurry (*)) . skipPairs False
  where
    skipPairs _ [] = []
    skipPairs skip (x : xs) = case x of
      Skip -> skipPairs True xs
      Take -> skipPairs False xs
      Pair a b
        | skip -> skipPairs skip xs
        | otherwise -> (a, b) : skipPairs skip xs

day :: Day
day = mkDay 3 parse solveA solveB
