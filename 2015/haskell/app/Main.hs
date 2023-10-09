module Main where

import Text.Printf (printf)
import qualified Day01


readDay :: Int -> IO String
readDay n = readFile $ getPath n
  where
    getPath n = "../.inputs/" <> printf "%02d" n <> ".txt"


main :: IO ()
main = do
  content <- readDay 1
  let parsed = Day01.parse content
  print $ Day01.solve1 parsed
  print $ Day01.solve2 parsed
