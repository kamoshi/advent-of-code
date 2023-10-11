module Main where

import Text.Printf (printf)
import qualified Day01
import qualified Day03

readDay :: Int -> IO String
readDay n = readFile $ getPath n
  where
    getPath n = "../.inputs/" <> printf "%02d" n <> ".txt"


main :: IO ()
main = do
  content <- readDay 3
  let parsed = Day03.parse content
  print $ Day03.solve1 parsed
  print $ Day03.solve2 parsed
