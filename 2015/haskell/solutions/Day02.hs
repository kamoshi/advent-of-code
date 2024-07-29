module Day02 (parse, solveA, solveB) where

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter xs =
  let (before, remainder) = break (== delimiter) xs
   in before : case remainder of
        [] -> []
        _ : rest -> splitOn delimiter rest

parse :: String -> [(Int, Int, Int)]
parse = map toTuple . lines
 where
  toTuple :: String -> (Int, Int, Int)
  toTuple str = let [a, b, c] = splitOn 'x' str in (read a, read b, read c)

toArea :: (Int, Int, Int) -> Int
toArea (l, w, h) =
  let a = l * w
      b = w * h
      c = h * l
   in 2 * a + 2 * b + 2 * c + min a (min b c)

solveA :: [(Int, Int, Int)] -> Int
solveA = sum . map toArea

-- >>> map toArea $ parse "2x3x4"
-- [64]

solveB = undefined
