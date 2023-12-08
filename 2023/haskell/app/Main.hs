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
import qualified Day08


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
  --run 1 Day01.parse Day01.solveA Day01.solveB
  --run 2 Day02.parse Day02.solveA Day02.solveB
  --run 3 Day03.parse Day03.solveA Day03.solveB
  --run 4 Day04.parse Day04.solveA Day04.solveB
  --run 5 Day05.parse Day05.solveA Day05.solveB
  --run 6 Day06.parse Day06.solveA Day06.solveB
  --run 7 Day07.parse Day07.solveA Day07.solveB
  run 8 Day08.parse Day08.solveA Day08.solveB
