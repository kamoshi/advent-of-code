import «Aoc2015»

def asChange : Char -> Int
  | '(' => 1
  | ')' => -1
  | _   => 0

def main : IO Unit := do
  let hello <- IO.FS.readFile "../.inputs/01.txt"
  let sum := hello |> String.toList |> List.map asChange |> List.foldl (.+.) 0
  IO.println sum
