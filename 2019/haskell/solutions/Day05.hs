{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Day05 (day) where

import Advent (Day, mkDay, (|>))
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Trans (STT, runSTT)
import Data.Array.Base (STUArray, readArray, writeArray)
import Data.Array.MArray (newListArray)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Text (Text)
import Data.Text qualified as Text

parse :: Text -> Either String [Int]
parse text =
  text
    |> Text.split (== ',')
    |> map (read @Int . Text.unpack)
    |> Right

type Memory s = (STUArray s Int Int, STRef s Int)

memInit :: [Int] -> ST s (Memory s)
memInit ns = do
  arr <- newListArray (0, length ns - 1) ns
  ptr <- newSTRef 0
  pure (arr, ptr)

memOffsetRead :: STUArray s Int Int -> Int -> ST s Int
memOffsetRead arr ptr = readArray arr ptr >>= readArray arr

memOffsetWrite :: STUArray s Int Int -> Int -> Int -> ST s ()
memOffsetWrite arr ptr new = readArray arr ptr >>= flip (writeArray arr) new

memOpSimple :: (Int -> Int -> Int) -> (STUArray s Int Int, STRef s Int) -> ST s ()
memOpSimple op (arr, ptr) = do
  ptrVal <- readSTRef ptr
  result <- liftA2 op (memOffsetRead arr (ptrVal + 1)) (memOffsetRead arr (ptrVal + 2))
  memOffsetWrite arr (ptrVal + 3) result
  modifySTRef ptr (+ 4)

memStep :: Memory s -> STT s IO (Memory s)
memStep tape = do
  ptr <- readSTRef (snd tape)
  opN <- readArray (fst tape) ptr
  case opN of
    1 -> do
      memOpSimple (+) tape
      memStep tape
    2 -> do
      memOpSimple (*) tape
      memStep tape
    -- 3 -> do
    --   input <- lift readInput
    --   dest <- lift $ readArray (fst tape) (ptr + 1)
    --   lift $ writeArray (fst tape) dest input
    --   memStep tape
    -- 4 -> do
    --   addr <- lift $ readArray (fst tape) (ptr + 1)
    --   val <- lift $ readArray (fst tape) addr
    --   lift $ writeOutput val
    --   memStep tape
    99 ->
      return tape
    _ ->
      error $ "Unknown opcode: " <> show opN

solveA :: [Int] -> Int
solveA ns = runST $ do
  (arr, ptr) <- memInit ns
  writeArray arr 1 12
  writeArray arr 2 2
  (array, _) <- memStep (arr, ptr)
  readArray array 0

solveB :: [Int] -> Int
solveB ns =
  [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]
    |> map runWith
    |> filter ((19690720 ==) . snd)
    |> head
    |> fst
  where
    runWith :: (Int, Int) -> (Int, Int)
    runWith (noun, verb) = runST $ do
      (arr, ptr) <- memInit ns
      writeArray arr 1 noun
      writeArray arr 2 verb
      (array, _) <- memStep (arr, ptr)
      result <- readArray array 0
      pure (100 * noun + verb, result)

day :: Day
day = mkDay 2 parse solveA solveB
