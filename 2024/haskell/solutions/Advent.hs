{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}

module Advent (Day, mkDay) where

import Data.Text (Text)

data Solution where
  Solution :: forall a b. (Show a, Show b) => a -> b -> Solution

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
