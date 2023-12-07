{-# LANGUAGE OverloadedStrings #-}
module Day07 (parse, solveA, solveB) where

import Data.Void (Void)
import Data.Char (isDigit, digitToInt)
import Data.Text (Text)
import Data.List (sort, find, sortBy)
import Data.Function (on)
import Data.Bifunctor (first, Bifunctor (bimap))
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, many, eof)
import Text.Megaparsec.Char (alphaNumChar, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad (join)


data Card = X | Plain Int | T | J | Q | K | A
  deriving (Eq, Ord, Read, Show)

type Hand = [Card]
type Play = (Hand, Int)

data HandType = High | OnePair | TwoPair | Three | Full | Four | Five
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text


parse :: Text -> Either String [Play]
parse = first errorBundlePretty . runParser plays ""
  where
    card :: Char -> Card
    card c
      | isDigit c = Plain $ digitToInt c
      | otherwise = read $ pure c
    play :: Parser Play
    play = do
      cs  <- map card <$> many alphaNumChar
      space
      bet <- decimal
      space
      return (cs, bet)
    plays :: Parser [Play]
    plays = many play <* eof

spansBy :: (a -> a -> Bool) -> [a] -> [[a]]
spansBy = helper []
  where
    helper :: [[a]] -> (a -> a -> Bool) -> [a] -> [[a]]
    helper acc        _ []     = reverse . map reverse $ acc
    helper []         p (x:xs) = helper [[x]] p xs
    helper acc@(a:as) p (x:xs) =
      if p (head a) x
      then helper ((x:a):as) p xs
      else helper ([x]:acc)  p xs

group :: Hand -> [[Card]]
group = spansBy (==) . sort

classify :: Hand -> HandType
classify cs
    | has [5]    = Five
    | has [4]    = Four
    | has [3, 2] = Full
    | has [3]    = Three
    | count 2 2  = TwoPair
    | has [2]    = OnePair
    | otherwise  = High
    where
      sizes :: [Int]
      sizes = map length . group $ cs
      has :: [Int] -> Bool
      has   = all (`elem` sizes)
      count :: Int -> Int -> Bool
      count n = (==) . length . filter (n==) $ sizes

withType :: Play -> (HandType, Play)
withType = first (classify . fst) . join (,)

score :: Int -> Play -> Int
score = flip $ (*) . snd

solveA :: [Play] -> Int
solveA = sum . zipWith score [1..] . map snd . sort . map withType

classify' :: [Card] -> HandType
classify' cs =
  let counts = map (bimap length head . join (,)) . spansBy (==) . sort $ cs
      jokers = maybe 0 fst . find ((J ==) . snd) $ counts
      others = sortBy (flip compare `on` fst) . filter ((J /=) . snd) $ counts
  in case others of
    []              -> Five
    ((n, c) : rest) -> classify . concatMap (uncurry replicate) $ (n + jokers, c) : rest

erase :: Card -> [Card] -> [Card]
erase e = helper
  where
    helper [] = []
    helper (c:cs)
      | e == c    = X : helper cs
      | otherwise = c : helper cs

withType' :: Play -> (HandType, Play)
withType' = bimap (classify' . fst) (first (erase J)) . join (,)

solveB :: [Play] -> Int
solveB = sum . zipWith score [1..] . map snd . sort . map withType'
