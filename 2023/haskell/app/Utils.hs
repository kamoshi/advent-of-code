module Utils (readInput) where

import Prelude hiding (readFile)
import Text.Printf (printf)
import qualified Data.Text as T
import Data.Text.IO (readFile)


readInput :: Int -> IO T.Text
readInput = readFile . printf "../.inputs/%02d"
