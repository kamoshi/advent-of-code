package day01

import kamlib.{Reader, Wrapper}

object Main {

  /** MAIN */
  def main(args: Array[String]): Unit =
  {
    val input: List[Int] = Reader.readList("/input1.txt").map(_.toInt)
    val solution: Day1 = new Day1

    println("Part 1")
    Wrapper(solution.solveP1(input)).print()
    println("Part 2")
    Wrapper(solution.solveP2(input)).print()
  }

}
