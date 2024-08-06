{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Day05 (parse, solveA, solveB) where

import Control.Monad (foldM)
import Control.Monad qualified as T
import Data.List (tails)
import Data.Maybe (fromMaybe)
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

checkTriple :: Text -> Bool
checkTriple text
  | T.length text > 2 = T.index text 0 == T.index text 2
  | otherwise = False

checkPairs :: Text -> Bool
checkPairs "" = False
checkPairs text =
  let (cs, rest) = T.splitAt 2 text
   in cs `T.isInfixOf` rest

isNiceB :: Text -> Bool
isNiceB text =
  let parts = T.tails text
   in any checkTriple parts && any checkPairs parts

solveB :: [Text] -> Int
solveB = length . filter isNiceB
