{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import Text.Printf (printf)

readDay :: Int -> IO String
readDay n = readFile $ getPath n
 where
  getPath n = "../.inputs/" <> printf "%02d" n

main :: IO ()
main = do
  content <- readDay 2
  let parsed = Day02.parse content
  print $ Day02.solveA parsed
  print @Int $ Day02.solveB parsed
