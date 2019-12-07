package day07

import kamlib.{Reader, Wrapper}

object Main {

  def main(args: Array[String]): Unit =
  {
    val input: Array[Int] = Reader.readString("/input7.txt").split("[^\\d-]+").map(x => x.toInt)
    val solution: Day07 = new Day07(input)

    println("Part 1:")
    Wrapper(solution.solveP1()).print()
    println("Part 2:")
    Wrapper(solution.solveP2()).print()
  }
}
