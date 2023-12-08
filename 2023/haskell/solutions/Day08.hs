{-# LANGUAGE OverloadedStrings #-}
module Day08 (parse, solveA, solveB) where

import Data.Void (Void)
import Data.Text (Text)
import Data.List (transpose)
import Data.Bifunctor (first)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, many, eof)
import Text.Megaparsec.Char (alphaNumChar, space, char)


data Dir = L | R
  deriving (Show, Read)

type Node  = (String, (String, String))
type Input = ([Dir], [Node])

type Parser = Parsec Void Text


parse :: Text -> Either String ([Dir], [Node])
parse = first errorBundlePretty . runParser input ""
  where
    node :: Parser Node
    node = do
      name <- many alphaNumChar
      space
      _    <- char '='
      space
      _    <- char '('
      l    <- many alphaNumChar
      _    <- char ','
      space
      r    <- many alphaNumChar
      _    <- char ')'
      space
      return (name, (l, r))
    input :: Parser ([Dir], [Node])
    input = do
      path <- map (read . pure) <$> many alphaNumChar
      space
      ns   <- many node
      eof
      return (path, ns)

run :: [Node] -> String -> [Dir] -> [(Int, String)]
run ns start = helper 0 start . cycle
  where
    get :: String -> (String, String)
    get name = snd . head . filter ((name ==) . fst) $ ns
    helper :: Int -> String -> [Dir] -> [(Int, String)]
    helper i curr dir = (i, curr) : helper (succ i) (move $ get curr) (tail dir)
      where
        move = case head dir of
          L -> fst
          R -> snd

solveA :: Input -> Int
solveA (ds, ns) = fst . head . filter (("ZZZ" ==) . snd) $ run ns "AAA" ds

solveB :: Input -> Int
solveB (ds, ns) = frequency . head . transpose . map (filter (('Z' ==) . last . snd)) $ toParallel ns
  where
    toParallel :: [Node] -> [[(Int, String)]]
    toParallel = map (($ ds) . run ns) . filter (('A' ==) . last) . map fst
    frequency :: [(Int, String)] -> Int
    frequency = foldl lcm 1 . map fst
