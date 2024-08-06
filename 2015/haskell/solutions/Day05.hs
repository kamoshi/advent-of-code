{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Day05 (parse, solveA) where

import Control.Monad (foldM)
import Control.Monad qualified as T
import Data.Either (fromLeft)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T

parse :: Text -> [Text]
parse = T.lines

isVovel :: Char -> Bool
isVovel c = elem @[] c "aeiou"

isWrong :: String -> Bool
isWrong cs = cs `elem` ["ab", "cd", "pq", "xy"]

isNice :: Text -> Bool
isNice = maybe False (check . snd) . foldM next ('_', (False, 0)) . T.unpack
 where
  check :: (Bool, Int) -> Bool
  check (double, vowels) = double && vowels >= 3
  next :: ((Char, (Bool, Int)) -> Char -> Maybe (Char, (Bool, Int)))
  next (prev, (double, vowels)) char =
    let double' = double || prev == char
        vowels' = if isVovel char then vowels + 1 else vowels
     in if isWrong [prev, char]
          then Nothing
          else Just (char, (double', vowels'))

solveA :: [Text] -> Int
solveA = length . filter isNice

test :: Text
test = "dvszwmarrgswjxmbau"

-- $> isNice test

solveB = undefined
