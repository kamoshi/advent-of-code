module Day01 where
import Data.List (groupBy, sort)


inputPath :: FilePath
inputPath = "../.inputs/day01.txt"

sample :: [String]
sample =
  [ "1000"
  , "2000"
  , "3000"
  , ""
  , "4000"
  , ""
  , "5000"
  , "6000"
  , ""
  , "7000"
  , "8000"
  , "9000"
  , ""
  , "10000"
  ]


groupData :: [String] -> [[String]]
groupData = map (filter (/= "")) . groupBy (const (/=""))

parseGroups :: [[String]] -> [[Integer]]
parseGroups = map $ map read

readPuzzle :: IO [[Integer]]
readPuzzle = parseGroups . groupData . lines <$> readFile inputPath

solve1 :: [[Integer]] -> Integer
solve1 = maximum . map sum

solve2 :: [[Integer]] -> Integer
solve2 = sum . take 3 . reverse . sort . map sum

main :: IO ()
main = do
  puzzle <- readPuzzle
  print $ solve1 puzzle
  print $ solve2 puzzle

