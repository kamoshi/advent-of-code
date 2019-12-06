package day06

import kamlib.{Reader, Wrapper}

object Main {

  def main(args: Array[String]): Unit = {
    val input: List[String] = Reader.readList("/input6.txt")
    val tuple = Wrapper(new Day06(input)).tuple
    println(s"Time initializing data structures: ${tuple._2}ms")
    val solution = tuple._1

    println("Part 1:")
    Wrapper(solution.solveP1()).print()
    println("Part 2:")
    Wrapper(solution.solveP2()).print()
  }
}
