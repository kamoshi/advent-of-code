{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Day04 (solveA, solveB) where

import Crypto.Hash.MD5 (hash)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Text.Printf (printf)

hashHex :: ByteString -> String
hashHex = concatMap (printf "%02x") . BS.unpack . hash

test :: ByteString
test = "ckczppom"

solve :: Int -> ByteString -> Integer
solve zeros key = fst . head . filter (isMatch . snd) . map toHash $ [1 ..]
 where
  toHash n = (n, hashHex $ key <> Char8.pack (show n))
  isMatch = all (== '0') . take zeros

solveA :: ByteString -> Integer
solveA = solve 5

solveB :: ByteString -> Integer
solveB = solve 6
