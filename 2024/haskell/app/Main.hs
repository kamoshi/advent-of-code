{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T (readFile)
import Text.Printf (printf)

{- ORMOLU_DISABLE -}
import Advent (Day)
import Day01 qualified as Day01
import Day02 qualified as Day02
import Day03 qualified as Day03
{- ORMOLU_ENABLE -}

readInput :: Int -> IO T.Text
readInput = T.readFile . printf "../.input/%02d"

days :: [Day]
days =
  [ Day01.day,
    Day02.day,
    Day03.day
  ]

main :: IO ()
main = do
  forM_ days $ \(day, run) -> do
    putStrLn $ "Day " <> show day
    text <- readInput day

    case run text of
      Left err -> putStrLn $ "ERROR: " <> err
      Right solution -> print solution
