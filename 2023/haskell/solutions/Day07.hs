{-# LANGUAGE OverloadedStrings #-}
module Day07 (parse, solveA, solveB) where

import Data.Void (Void)
import Data.Char (isDigit, digitToInt)
import Data.Text (Text)
import Data.List (sort, find, sortBy)
import Data.Function (on)
import Data.Bifunctor (first, bimap, second)
import Control.Monad (join)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, many, eof)
import Text.Megaparsec.Char (alphaNumChar, space)
import Text.Megaparsec.Char.Lexer (decimal)


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
      | isDigit c = Plain . digitToInt $ c
      | otherwise = read . pure $ c
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
spansBy p = helper []
  where
    helper acc        []     = reverse . map reverse $ acc
    helper []         (x:xs) = helper [[x]] xs
    helper acc@(a:as) (x:xs) =
      if p (head a) x
      then helper ((x:a):as) xs
      else helper ([x]:acc)  xs

group :: Hand -> [[Card]]
group = spansBy (==) . sort

classify :: Hand -> HandType
classify = cast . sortBy (flip compare) . map length . group
  where
    cast :: [Int] -> HandType
    cast ns = case ns of
      5:_   -> Five
      4:_   -> Four
      3:2:_ -> Full
      3:_   -> Three
      2:2:_ -> TwoPair
      2:_   -> OnePair
      _     -> High

withType :: (Hand -> HandType) -> Play -> (HandType, Play)
withType toType = first (toType . fst) . join (,)

score :: Int -> Play -> Int
score = flip $ (*) . snd

solveA :: [Play] -> Int
solveA = sum . zipWith score [1..] . map snd . sort . map (withType classify)

classify' :: Hand -> HandType
classify' cs =
  let counts = map (bimap length head . join (,)) . group $ cs
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

solveB :: [Play] -> Int
solveB = sum . zipWith score [1..] . map snd . sort . map (second (first $ erase J) . withType classify')
