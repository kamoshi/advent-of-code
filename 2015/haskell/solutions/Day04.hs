module Day04 (solveA, solveB) where

import Crypto.Hash.MD5 (hash)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Text.Printf (printf)

hashHex :: String -> String
hashHex = concatMap (printf "%02x") . BS.unpack . hash . Char8.pack

test :: String
test = "ckczppom"

solve :: Int -> String -> Integer
solve zeros key = fst . head . filter (isMatch . snd) . map toHash $ [1 ..]
 where
  toHash n = (n, hashHex $ key <> show n)
  isMatch = all (== '0') . take zeros

solveA :: String -> Integer
solveA = solve 5

solveB :: String -> Integer
solveB = solve 6
