{-# LANGUAGE OverloadedStrings #-}
module Day10 where

import Data.Void (Void)
import Data.Text (Text)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first, bimap)
import Control.Monad (join)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, many, eof, satisfy)
import Text.Megaparsec.Char (newline)
import Misc (withCoords, pairs)


data Pipe
  = E -- empty
  | S -- start
  | I -- vertical
  | H -- horizontal
  | J -- up-left
  | L -- up-right
  | T -- down-left
  | F -- down-right
  deriving (Show, Eq)

data Dir = DLeft | DRight | DUp | DDown
  deriving (Show, Eq)

type Row  = Int
type Col  = Int
type Move = (Dir, (Row, Col))

type Parser = Parsec Void Text


parse :: Text -> Either String [[Pipe]]
parse = first errorBundlePretty . runParser grid ""
  where
    pipe :: Char -> Pipe
    pipe c = case c of
      '.' -> E
      'S' -> S
      '|' -> I
      '-' -> H
      'J' -> J
      'L' -> L
      '7' -> T
      'F' -> F
      bad -> error $ "Invalid character: " <> [bad]
    row :: Parser [Pipe]
    row = map pipe <$> many (satisfy $ not . isSpace) <* newline
    grid :: Parser [[Pipe]]
    grid = many row <* eof

findStart :: [[Pipe]] -> (Row, Col)
findStart = fst . head . filter ((S ==) . snd) . withCoords

get :: (Row, Col) -> [[Pipe]] -> Pipe
get (r, c) = snd . head . filter ((c ==) . fst) . zip [0..] . snd . head . filter ((r ==) . fst) . zip [0..]

nextD :: Dir -> Pipe -> Maybe Dir
nextD dir pipe = case (pipe, dir) of
  (I, DDown)  -> Just DDown
  (I, DUp)    -> Just DUp
  (H, DRight) -> Just DRight
  (H, DLeft)  -> Just DLeft
  (J, DDown)  -> Just DLeft
  (J, DRight) -> Just DUp
  (L, DDown)  -> Just DRight
  (L, DLeft)  -> Just DUp
  (T, DUp)    -> Just DLeft
  (T, DRight) -> Just DDown
  (F, DUp)    -> Just DRight
  (F, DLeft)  -> Just DDown
  _           -> Nothing

nextC :: (Row, Col) -> Dir -> (Row, Col)
nextC (r, c) dir = case dir of
  DLeft  -> (r, c - 1)
  DRight -> (r, c + 1)
  DUp    -> (r - 1, c)
  DDown  -> (r + 1, c)

starts :: [[Pipe]] -> (Move, Move)
starts grid =
  let s  = findStart grid
      ds = mapMaybe (\dir -> check dir $ nextC s dir) [ DUp, DDown, DLeft, DRight ]
  in (head ds, head $ tail ds)
  where
    check :: Dir -> (Row, Col) -> Maybe Move
    check dir rc = fmap (const (dir, rc)) . nextD dir $ get rc grid

go :: [[Pipe]] -> (Dir, (Row, Col)) -> [(Dir, (Row, Col))]
go grid = helper
  where
    helper :: (Dir, (Row, Col)) -> [(Dir, (Row, Col))]
    helper (dir, rc) = (dir, rc) : case nextD dir $ get rc grid of
      Just dir' -> helper (dir', nextC rc dir')
      Nothing   -> []

solveA :: [[Pipe]] -> Int
solveA grid = (1 +) . length . takeWhile (not . isMatch) . uncurry zip . join bimap (go grid) $ starts grid
  where
    isMatch :: (Move, Move) -> Bool
    isMatch (l, r) = snd l == snd r

verts :: [[Pipe]] -> ((Row, Col), [(Row, Col)])
verts grid = (findStart grid, findVerts grid)
  where
    isVertex :: (Move, Move) -> Maybe (Row, Col)
    isVertex ((dir, rc), (dir', _)) = if dir /= dir' then Just rc else Nothing
    findVerts :: [[Pipe]] -> [(Row, Col)]
    findVerts = mapMaybe isVertex . pairs . go grid . fst . starts

-- https://en.wikipedia.org/wiki/Shoelace_formula
-- A = ((x1 * y2 - y1 * x2) + (x2 * y3 - x3 * y2) + ...) / 2
trapezoid :: ((Row, Col), [(Row, Col)]) -> Double
trapezoid (sp, vs) = (/2) . fromIntegral . sum . map inner . pairs $ [sp] <> vs <> [sp]
  where
    inner :: ((Row, Col), (Row, Col)) -> Int
    inner ((r1, c1), (r2, c2)) = r1 * c2 - r2 * c1

-- https://en.wikipedia.org/wiki/Pick%27s_theorem
-- A = i + b/2 - 1
-- i = A - b/2 + 1
interior :: ((Row, Col), [(Row, Col)]) -> Int -> Double
interior vs path = abs $ trapezoid vs - fromIntegral path + 1

solveB :: [[Pipe]] -> Double
solveB grid = interior (verts grid) (solveA grid)
