{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified
import Day05 qualified
import Text.Printf (printf)

readDay :: Int -> IO Text
readDay n = TIO.readFile $ getPath n
 where
  getPath n = "../.inputs/" <> printf "%02d" n

main :: IO ()
main = do
  content <- Day05.parse <$> readDay 5
  print $ Day05.solveA content
  print $ Day05.solveB content

-- print @Int $ Day05.solveB parsed
