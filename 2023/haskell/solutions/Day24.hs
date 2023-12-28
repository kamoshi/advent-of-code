{-# LANGUAGE OverloadedStrings #-}
module Day24 (parse, solveA, solveB) where

import Data.Void (Void)
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first, bimap)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, eof, many, sepBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (signed, decimal)
import Numeric.LinearAlgebra (R, (|>), linearSolve, asColumn, fromColumns, flatten, toList, fromLists, col)
import Misc (paired)


type Ray  = ([R], [R])

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

findThrow :: [Ray] -> Maybe [Int]
findThrow rays =
  let
    ([sx1, sy1, sz1], [vx1, vy1, vz1]) = rays !! 0
    ([sx2, sy2, sz2], [vx2, vy2, vz2]) = rays !! 1
    ([sx3, sy3, sz3], [vx3, vy3, vz3]) = rays !! 2
    consts = fromLists
      [ [0, vz2 - vz1, vy1 - vy2, 0, sz1 - sz2, sy2 - sy1]
      , [0, vz3 - vz1, vy1 - vy3, 0, sz1 - sz3, sy3 - sy1]
      , [vz1 - vz2, 0, vx2 - vx1, sz2 - sz1, 0, sx1 - sx2]
      , [vz1 - vz3, 0, vx3 - vx1, sz3 - sz1, 0, sx1 - sx3]
      , [vy2 - vy1, vx1 - vx2, 0, sy1 - sy2, sx2 - sx1, 0]
      , [vy3 - vy1, vx1 - vx3, 0, sy1 - sy3, sx3 - sx1, 0]
      ]
    coeffs = col
      [ vy1 * sz1 - sy1 * vz1 + sy2 * vz2 - vy2 * sz2
      , vy1 * sz1 - sy1 * vz1 + sy3 * vz3 - vy3 * sz3
      , sx1 * vz1 - vx1 * sz1 + vx2 * sz2 - sx2 * vz2
      , sx1 * vz1 - vx1 * sz1 + vx3 * sz3 - sx3 * vz3
      , vx1 * sy1 - sx1 * vy1 + sx2 * vy2 - vx2 * sy2
      , vx1 * sy1 - sx1 * vy1 + sx3 * vy3 - vx3 * sy3
      ]
    in map round . toList . flatten <$> linearSolve consts coeffs

solveB :: [Ray] -> Int
solveB = maybe 0 (sum . take 3) . findThrow
