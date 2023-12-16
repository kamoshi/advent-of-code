{-# LANGUAGE OverloadedStrings #-}
module Day15 (parse, solveA, solveB) where

import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (ord, isSpace)
import Data.Bifunctor (bimap)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, sepBy, optional, eof, many, choice)
import Text.Megaparsec.Char (newline, alphaNumChar, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Lens.Micro (ix, (%~))


data Op
  = Set Text Int
  | Del Text
  deriving Show

type Entry = (Text, Int)

type Parser = Parsec Void Text


parse :: Text -> Either String [(Text, Op)]
parse text = bimap errorBundlePretty (zip . simple $ text) . runParser complex "" $ text
  where
    simple :: Text -> [Text]
    simple = T.splitOn "," . T.filter (not . isSpace)
    single :: Parser Op
    single = do
      label  <- T.pack <$> many alphaNumChar
      choice
        [ Set label <$> (char '=' *> decimal)
        , Del label <$  char '-'
        ]
    complex :: Parser [Op]
    complex = single `sepBy` "," <* optional newline <* eof

hash :: Text -> Int
hash = T.foldl step 0
  where
    step :: Int -> Char -> Int
    step curr = (`rem` 256) . (17 *) . (curr +) . ord

solveA :: [(Text, Op)] -> Int
solveA = sum . map (hash . fst)

del :: Text -> [[Entry]] -> [[Entry]]
del k = ix (hash k) %~ filter ((k /=) . fst)

set :: Text -> Int -> [[Entry]] -> [[Entry]]
set k v = ix (hash k) %~ update
  where
    update :: [Entry] -> [Entry]
    update es
      | any ((k ==) . fst) es = map (\(k', v') -> if k == k' then (k, v) else (k', v')) es
      | otherwise = es <> [(k, v)]

solveB :: [(Text, Op)] -> Int
solveB = sum . zipWith row [1..] . foldl (flip run) (replicate 256 []) . map snd
  where
    run :: Op -> [[Entry]] -> [[Entry]]
    run op = case op of
      Set k v -> set k v
      Del k   -> del k
    box :: Int -> Entry -> Int
    box i = (i *) . snd
    row :: Int -> [Entry] -> Int
    row i = (i *) . sum . zipWith box [1..]
