module Day02 (parse, solveA, solveB) where

import Data.List (sort, tails)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter xs =
  let (before, remainder) = break (== delimiter) xs
   in before : case remainder of
        [] -> []
        _ : rest -> splitOn delimiter rest

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

parse :: String -> [[Int]]
parse = map (map read . splitOn 'x') . lines

toArea :: [Int] -> Int
toArea = calculate . map (uncurry (*)) . pairs
 where
  calculate :: [Int] -> Int
  calculate sides = (sum . map (2 *)) sides + minimum sides

solveA :: [[Int]] -> Int
solveA = sum . map toArea

solveB :: [[Int]] -> Int
solveB = sum . map calcWrap
 where
  calcWrap :: [Int] -> Int
  calcWrap dims =
    let wrap = (2 *) . sum . take 2 $ sort dims
        bow = product dims
     in wrap + bow
