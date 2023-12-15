{-# LANGUAGE OverloadedStrings #-}
module Day15 (parse, solveA) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (ord, isSpace)


input :: Text
input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"


parse :: Text -> Either String [Text]
parse = Right . T.splitOn "," . T.filter (not . isSpace)

hash :: Text -> Int
hash = foldl step 0 . T.unpack
  where
    step :: Int -> Char -> Int
    step curr = (`rem` 256) . (17 *) . (curr +) . ord

solveA :: [Text] -> Int
solveA = sum . map hash



-- >>> solveA <$> parse input
-- Right 1320

