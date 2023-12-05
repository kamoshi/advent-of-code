{-# LANGUAGE OverloadedStrings #-}
module Day05 (parse, solveA, solveB) where

import Data.Void (Void)
import Data.Text (Text)
import Data.List (sort)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Data.Bifunctor (first)
import Control.Monad ((<=<))
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, many, sepBy, some, eof)
import Text.Megaparsec.Char (string, char, space, alphaNumChar)
import Text.Megaparsec.Char.Lexer (decimal)


type Target = Int
type Source = Int
type Range  = Int
type Trans  = ((Target, Source), Range)
type Recipe = ((String, String), [Trans])
type Input  = ([Int], [Recipe])

type Parser = Parsec Void Text


parse :: Text -> Either String Input
parse = first errorBundlePretty . runParser input ""
  where
    seeds :: Parser [Int]
    seeds = string "seeds: " *> decimal `sepBy` char ' ' <* space
    trans :: Parser Trans
    trans = do
      t     <- decimal
      space
      s     <- decimal
      space
      n     <- decimal
      space
      return ((t, s), n)
    recipe :: Parser Recipe
    recipe = do
      f    <- some alphaNumChar
      _    <- string "-to-"
      t    <- some alphaNumChar
      _    <- string " map:"
      space
      ms   <- many trans
      space
      return ((f, t), ms)
    input :: Parser Input
    input = do
      ss   <- seeds
      rs   <- many recipe
      eof
      return (ss, rs)


transform :: Trans -> Int -> Maybe Int
transform ((t, s), r) n
  | n >= s && n < s + r = Just $ n - s + t
  | otherwise           = Nothing

applyRecipe :: Int -> Recipe -> Int
applyRecipe n = fromMaybe n . listToMaybe . mapMaybe (($ n) . transform) . snd

foldRecipes :: Int -> [Recipe] -> Int
foldRecipes = foldl applyRecipe

solveA :: Input -> Int
solveA (ss, rs) = minimum . map (($ rs) . foldRecipes) $ ss

pairs :: [a] -> [(a, a)]
pairs []       = []
pairs (x:y:xs) = (x, y) : pairs xs
pairs _        = undefined

toRange :: (Int, Int) -> (Int, Int)
toRange (a, b) = (a, a + b -1)

unique :: [(Int, Int)] -> [(Int, Int)]
unique = merge . sort
  where
    merge :: [(Int, Int)] -> [(Int, Int)]
    merge []  = []
    merge [x] = [x]
    merge ((s1, e1) : (s2, e2) : rs)
      | e1 >= s2  = merge $ (s1, max e1 e2) : rs
      | otherwise = (s1, e1) : merge ((s2, e2) : rs)

toList :: (Int, Int) -> [Int]
toList (a, b) = [a .. b]

solveB :: Input -> Int
solveB (ss, rs) = solveA (expand ss, rs)
  where
    expand = toList <=< unique . map toRange . pairs
