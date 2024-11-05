module Utils where

import Data.Text (Text)

data Solution a b c = Solution
  { parse :: Text -> a
  , solveA :: a -> b
  , solveB :: a -> c
  }
