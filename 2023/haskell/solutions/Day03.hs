{-# LANGUAGE OverloadedStrings #-}
module Day03 (parse, solveA, solveB) where

import Data.Bifunctor (first, second)
import Data.Void (Void)
import Data.Char (digitToInt, isSpace)
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Text.Megaparsec (errorBundlePretty, Parsec, runParser, many, eof, choice, satisfy, manyTill)
import Text.Megaparsec.Char (digitChar, char)


data Item
  = Digit Int
  | Symbol Char
  | Empty
  deriving (Show, Eq)

type Grid = [[Item]]
type Row = Int
type Col = Int

type Parser = Parsec Void Text


parse :: Text -> Either String Grid
parse = first errorBundlePretty . runParser full ""
  where
    item :: Parser Item
    item = choice
      [ Digit . digitToInt <$> digitChar
      , Empty  <$  char '.'
      , Symbol <$> satisfy (not . isSpace)
      ]
    line :: Parser [Item]
    line = item `manyTill` satisfy isSpace
    full :: Parser [[Item]]
    full = many line <* eof

findSpans :: (a -> a -> Bool) -> [a] -> [[a]]
findSpans = helper []
  where
    helper :: [[a]] -> (a -> a -> Bool) -> [a] -> [[a]]
    helper acc        _ []     = reverse . map reverse $ acc
    helper []         p (x:xs) = helper [[x]] p xs
    helper acc@(a:as) p (x:xs) =
      if p (head a) x
      then helper ((x:a):as) p xs
      else helper ([x]:acc)  p xs

withCoords :: [[a]] -> [((Row, Col), a)]
withCoords grid = [((r, c), a) | (r, row) <- zip [0..] grid, (c, a) <- zip [0..] row]

getSymbols :: Grid -> [(Char, (Row, Col))]
getSymbols = mapMaybe isSymbol . withCoords
  where
    isSymbol :: ((Row, Col), Item) -> Maybe (Char, (Row, Col))
    isSymbol (rc, item) = case item of
      Symbol s -> Just (s, rc)
      _        -> Nothing

getNumbers :: Grid -> [(Int, (Row, Col, Col))]
getNumbers = concat . zipWith processRow [0..]
  where
    getDigit :: (Col, Item) -> Maybe (Col, Int)
    getDigit (i, item) = case item of
      Digit d -> Just (i, d)
      _       -> Nothing
    isNext :: (Col, Int) -> (Col, Int) -> Bool
    isNext a b = fst a + 1 == fst b
    merge :: Row -> [(Col, Int)] -> (Int, (Row, Col, Col))
    merge r items =
      let n = foldl (\acc (_, d) -> 10 * acc + d) 0 items
          s = minimum $ map fst items
          e = maximum $ map fst items
      in (n, (r, s, e))
    processRow :: Row -> [Item] -> [(Int, (Row, Col, Col))]
    processRow r = map (merge r) . findSpans isNext . mapMaybe getDigit . zip [0..]

getNeigbors :: (Row, Col, Col) -> [(Row, Col)]
getNeigbors (r, s, e) = [(r, s-1), (r, e+1)]
  <> [(r-1, c) | c <- [s-1..e+1]]
  <> [(r+1, c) | c <- [s-1..e+1]]

solveA :: Grid -> Int
solveA grid =
  let symbols = map snd $ getSymbols grid
      numbers = getNumbers grid
  in sum . map fst . filter (hasSymbol symbols . getNeigbors . snd) $ numbers
  where
    hasSymbol :: [(Row, Col)] -> [(Row, Col)] -> Bool
    hasSymbol symbols = any (`elem` symbols)

solveB :: Grid -> Int
solveB grid =
  let symbols = map snd . filter (('*'==) . fst) $ getSymbols grid
      numbers = map (second getNeigbors) $ getNumbers grid
  in sum . map product . filter ((2==) . length) . map (findNumbers numbers) $ symbols
  where
    findNumbers :: [(Int, [(Row, Col)])] -> (Row, Col) -> [Int]
    findNumbers ns symbol = map fst . filter ((symbol `elem`) . snd) $ ns

