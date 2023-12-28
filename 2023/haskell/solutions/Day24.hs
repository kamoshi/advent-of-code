{-# LANGUAGE OverloadedStrings #-}
module Day24 (parse, solveA, solveB) where

import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, eof, many, sepBy)
import Data.Bifunctor (first, Bifunctor (bimap))
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (signed, decimal)
import Numeric.LinearAlgebra (Matrix, R, (|>), (><), linearSolve, asColumn, fromColumns, flatten, toList)
import Misc (paired)
import Data.Maybe (mapMaybe)


type Ray  = ([R], [R])


input :: Text
input =
  "19, 13, 30 @ -2,  1, -2\n\
  \18, 19, 22 @ -1, -1, -2\n\
  \20, 25, 34 @ -2, -2, -4\n\
  \12, 31, 28 @ -1, -2, -1\n\
  \20, 19, 15 @  1, -5, -3\n"

type Parser = Parsec Void Text

parse :: Text -> Either String [Ray]
parse = first errorBundlePretty . runParser rays ""
  where
    point :: Parser [R]
    point = signed space decimal `sepBy` (char ',' <* space)
    ray :: Parser ([R], [R])
    ray = do
      t1 <- point
      space
      _  <- char '@'
      space
      t2 <- point
      space
      return (t1, t2)
    rays :: Parser [Ray]
    rays = many ray <* eof


intersect2D :: Ray -> Ray -> Maybe ([R], (R, R))
intersect2D (sa, va) (sb, vb) =
  let consts = 2 |> sa - 2 |> sb
      coeffs = [2 |> vb, negate $ 2 |> va]
      times  = toList . flatten <$> linearSolve (fromColumns coeffs) (asColumn consts)
  in case times of
    Just ts@(tb:_) -> Just (ts, toPoint (sb, vb) tb)
    _otherwise     -> Nothing
  where
    toPoint :: Ray -> R -> (R, R)
    toPoint (sx:sy:_, vx:vy:_) t = (sx + t * vx, sy + t * vy)
    toPoint _ _ = undefined

solveAFor :: R -> R -> [Ray] -> Int
solveAFor s e = length . filter isBoth . mapMaybe (uncurry intersect2D) . paired
  where
    isFuture :: [R] -> Bool
    isFuture = all (0<=)
    isWithin :: (R, R) -> Bool
    isWithin (x, y) = s <= x && x <= e && s <= y && y <= e
    isBoth :: ([R], (R, R)) -> Bool
    isBoth = uncurry (&&) . bimap isFuture isWithin

solveA :: [Ray] -> Int
solveA = solveAFor 200000000000000 400000000000000

-- >>> head . paired <$> parse input
-- Right (([19.0,13.0,30.0],[-2.0,1.0,-2.0]),([18.0,19.0,22.0],[-1.0,-1.0,-2.0]))

-- >>> solveA <$> parse input
-- Right 2

solveB = const 1

