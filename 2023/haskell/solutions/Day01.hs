{-# LANGUAGE OverloadedStrings #-}
module Day01 (parse, solveA, solveB) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe, listToMaybe)


parse :: Text -> Either String [Text]
parse = Right . T.lines

merge :: [Int] -> Int
merge xs = 10 * head xs + last xs

solveA :: [Text] -> Int
solveA = sum . map (merge . convert)
  where
    convert :: Text -> [Int]
    convert = map (read . pure) . T.unpack . T.filter isDigit

replaces :: [(Text, Int)]
replaces =
  [ ("one",   1)
  , ("two",   2)
  , ("three", 3)
  , ("four",  4)
  , ("five",  5)
  , ("six",   6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine",  9)
  ]

solveB :: [Text] -> Int
solveB = sum . map (merge . convert)
  where
    -- If `text` starts with `prefix` return `Just n`, otherwise return `Nothing`
    tryReplace :: Text -> (Text, Int) -> Maybe Int
    tryReplace text (prefix, n)
      | prefix `T.isPrefixOf` text = Just n
      | otherwise = Nothing
    -- If `firstChar` is a digit return it, otherwise try replace the prefix
    parsePrefix :: Text -> Maybe Int
    parsePrefix text
      | isDigit firstChar = Just . read . pure $ firstChar
      | otherwise = listToMaybe $ mapMaybe (tryReplace text) replaces
      where
        firstChar = T.head text
    convert :: Text -> [Int]
    convert = mapMaybe parsePrefix . filter (not . T.null) . T.tails
