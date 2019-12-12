package day12

import kamlib.{Reader, Wrapper}

object Main {

  def main(args: Array[String]): Unit = {
    val solution: Day12 = new Day12()

    println("Part 1:")
    Wrapper(solution.solveP1()).print()
    println("Part 2:")
    Wrapper(solution.solveP2()).print()

  }
}
