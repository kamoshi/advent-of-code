module Main where

import Utils (readInput)
import qualified Day01
import qualified Day02


day01 :: IO ()
day01 = do
  input <- Day01.parse <$> readInput 1
  print . Day01.solveA $ input
  print . Day01.solveB $ input

day02 :: IO ()
day02 = do
  text <- readInput 2
  case Day02.parse text of
    Left err -> putStrLn err
    Right xd -> do
      print . Day02.solveA $ xd
      print . Day02.solveB $ xd

main :: IO ()
main = do
  --day01
  day02
