{-# LANGUAGE GADTs #-}

module Advent (Day, mkDay, exec, (|>)) where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)

infixl 1 |>

(|>) :: a -> (a -> b) -> b
(|>) = (&)

exec :: Parsec Void Text a -> Text -> Either String a
exec parser = first errorBundlePretty . runParser parser ""

data Solution where
  Solution :: (Show a, Show b) => a -> b -> Solution

instance Show Solution where
  show (Solution a b) = "A: " <> show a <> "\nB: " <> show b

{- ORMOLU_DISABLE -}
type Parser a   = Text -> Either String a
type Solver a b = a    -> b
type Runner     = Text -> Either String Solution
type Day        = (Int, Runner)
{- ORMOLU_ENABLE -}

mkDay :: (Show b, Show c) => Int -> Parser a -> Solver a b -> Solver a c -> Day
mkDay day parse solveA solveB = (day, run)
  where
    run input = do
      text <- parse input
      let a = solveA text
      let b = solveB text
      pure $ Solution a b
