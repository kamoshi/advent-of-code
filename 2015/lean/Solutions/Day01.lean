namespace Day01

def asChange : Char -> Int
  | '(' => 1
  | ')' => -1
  | _   => 0

def solve1 (input: String): Int :=
  input
    |> String.toList
    |> List.map asChange
    |> List.foldl (.+.) 0
