module Main where

import Data.Text (Text)
import Utils (readInput)
--import qualified Day01
--import qualified Day02
--import qualified Day03
--import qualified Day04
--import qualified Day05
--import qualified Day06
--import qualified Day07
--import qualified Day08
--import qualified Day09
--import qualified Day10
import qualified Day11
--import qualified Day15


run :: (Show b, Show c)
  => Int
  -> (Text -> Either String a)
  -> (a -> b)
  -> (a -> c)
  -> IO ()
run day parse solveA solveB = do
  text <- readInput day
  case parse text of
    Left err -> putStrLn err
    Right re -> do
      putStrLn $ "Day " <> show day
      print . solveA $ re
      print . solveB $ re

main :: IO ()
main = do
  --run 01 Day01.parse Day01.solveA Day01.solveB
  --run 02 Day02.parse Day02.solveA Day02.solveB
  --run 03 Day03.parse Day03.solveA Day03.solveB
  --run 04 Day04.parse Day04.solveA Day04.solveB
  --run 05 Day05.parse Day05.solveA Day05.solveB
  --run 06 Day06.parse Day06.solveA Day06.solveB
  --run 07 Day07.parse Day07.solveA Day07.solveB
  --run 08 Day08.parse Day08.solveA Day08.solveB
  --run 09 Day09.parse Day09.solveA Day09.solveB
  --run 10 Day10.parse Day10.solveA Day10.solveB
  run 11 Day11.parse Day11.solveA Day11.solveA
  --run 15 Day15.parse Day15.solveA Day15.solveB
