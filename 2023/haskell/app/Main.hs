module Main where

import qualified Data.Text as T
import Utils (readInput)

import qualified Day01
import qualified Day02

main :: IO ()
main = do
  text <- readInput 1
  putStrLn $ T.unpack text
