{-# LANGUAGE OverloadedStrings #-}
module Day10 (parse, solveA, solveB) where

import Data.Void (Void)
import Data.Text (Text)
import Data.Char (isSpace)
import Data.List (find)
import Data.Maybe (mapMaybe, catMaybes, fromJust)
import Data.Bifunctor (first, bimap)
import Control.Monad (join, (<=<))
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

data Dir = DirL | DirR | DirU | DirD
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

get :: (Row, Col) -> [[Pipe]] -> Maybe Pipe
get (r, c) = nth c <=< nth r
  where
    nth n = fmap snd . find ((n ==) . fst) . zip [0..]

nextD :: Dir -> Pipe -> Maybe Dir
nextD dir pipe = case (pipe, dir) of
  (I, DirD) -> Just DirD
  (I, DirU) -> Just DirU
  (H, DirR) -> Just DirR
  (H, DirL) -> Just DirL
  (J, DirD) -> Just DirL
  (J, DirR) -> Just DirU
  (L, DirD) -> Just DirR
  (L, DirL) -> Just DirU
  (T, DirU) -> Just DirL
  (T, DirR) -> Just DirD
  (F, DirU) -> Just DirR
  (F, DirL) -> Just DirD
  _         -> Nothing

nextC :: (Row, Col) -> Dir -> (Row, Col)
nextC (r, c) dir = case dir of
  DirL -> (r, c - 1)
  DirR -> (r, c + 1)
  DirU -> (r - 1, c)
  DirD -> (r + 1, c)

starts :: [[Pipe]] -> (Move, Move)
starts = do
  sp <- findStart
  ds <- checkDirs sp
  return (head ds, head $ tail ds)
  where
    check :: Dir -> (Row, Col) -> [[Pipe]] -> Maybe Move
    check dir rc = fmap (const (dir, rc)) . nextD dir <=< get rc
    checkDirs :: (Row, Col) -> [[Pipe]] -> [Move]
    checkDirs sp = do
      l <- check DirL $ nextC sp DirL
      r <- check DirR $ nextC sp DirR
      u <- check DirU $ nextC sp DirU
      d <- check DirD $ nextC sp DirD
      return $ catMaybes [l, r, u, d]

go :: [[Pipe]] -> (Dir, (Row, Col)) -> [(Dir, (Row, Col))]
go grid = helper
  where
    helper :: (Dir, (Row, Col)) -> [(Dir, (Row, Col))]
    helper (dir, rc) = (dir, rc) : case nextD dir . fromJust . get rc $ grid of
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
shoelace :: ((Row, Col), [(Row, Col)]) -> Double
shoelace (sp, vs) = (/2) . fromIntegral . abs . sum . map inner . pairs $ [sp] <> vs <> [sp]
  where
    inner :: ((Row, Col), (Row, Col)) -> Int
    inner ((r1, c1), (r2, c2)) = r1 * c2 - r2 * c1

-- https://en.wikipedia.org/wiki/Pick%27s_theorem
-- A = i + b/2 - 1
-- i = A - b/2 + 1
solveB :: [[Pipe]] -> Double
solveB = do
  vs <- verts
  hb <- solveA
  return $ shoelace vs - fromIntegral hb + 1
