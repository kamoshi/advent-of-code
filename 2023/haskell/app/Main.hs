module Main where

import Utils (readInput)
import qualified Day01


main :: IO ()
main = do
  text <- readInput 1
  print . Day01.solveA . Day01.parse $ text
  print . Day01.solveB . Day01.parse $ text
