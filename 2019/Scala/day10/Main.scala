package day10

import kamlib.{Reader, Wrapper}

object Main {

  def main(args: Array[String]): Unit = {
    val array: List[List[Char]] = Reader.readList("/input10.txt").map(_.toList)
    val solution: Day10 = new Day10(array)

    println("Part 1:")
    Wrapper(solution.solveP1()).print()
    println("Part 2:")
    Wrapper(solution.solveP2()).print()

  }
}
